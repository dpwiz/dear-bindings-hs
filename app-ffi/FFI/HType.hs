{-| Render a 'TypeKind' as a Haskell FFI type expression. Output is plain
'Text' (one line, no embedded newlines) suitable for splicing into a
generated @foreign import@ signature.

The mapping is fixed (see the FFI generator plan §B4) — no symbol
table is consulted; user-defined names ('TKUser') are emitted
verbatim and rely on the surrounding module bringing them into
scope.

Two helpers exposed for callers that want function-pointer-friendly
or argument-friendly views: 'renderArgType' for an 'Argument', and
'renderReturnType' for a 'TypeRef'.
-}
module FFI.HType
  ( renderHType
  , renderArgType
  , renderReturnType
  , typeKindContainsInlineAggregate
  , TypeAliasMap
  , typeKindUserNames
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Argument (..)
  , TypeKind (..)
  , TypeRef (..)
  )
import DearBindings.JSON.Types qualified

{- | Map from a C-side TKUser name (e.g. @VkDevice@) to the
@(module, haskell name)@ pair that resolves it. Pass-through for
anything not in the map (impl-owned or core-supplied names that
require no rename).
-}
type TypeAliasMap = Map Text (Text, Text)

{- | Render a 'TypeKind' tree as a Haskell type. The result is wrapped
in parentheses unless it's a single token, so it can always be
spliced inline (e.g. @\<inner\>@ inside @Ptr (\<inner\>)@).

The 'TypeAliasMap' renames TKUser names that are owned by external
Haskell packages (e.g. @VkDevice@ → @Device@ from @Vulkan.Core10@).
Names not in the map pass through verbatim.
-}
renderHType :: TypeAliasMap -> TypeKind -> Text
renderHType = render

renderReturnType :: TypeAliasMap -> TypeRef -> Text
renderReturnType aliases tr = renderHType aliases tr.description

{- | Render an 'Argument''s type as a Haskell type. Varargs ('isVarargs'
true, 'type_' is 'Nothing') don't have a representable Haskell type
and produce a placeholder; callers should filter them out at a
higher level.
-}
renderArgType :: TypeAliasMap -> Argument -> Text
renderArgType aliases a = case a.type_ of
  Just tr -> renderHType aliases tr.description
  Nothing -> "{- varargs unsupported -}"

-- ---------------------------------------------------------------------------
-- Internal

render :: TypeAliasMap -> TypeKind -> Text
render aliases = go
  where
    go = \case
      TKBuiltin name _scs -> renderBuiltin name
      TKPointer inner _nullable _ref _scs -> renderPointer aliases inner
      TKArray inner _bounds ->
        -- C arrays decay to pointers in function signatures and at struct
        -- field-by-pointer access; we treat them uniformly as Ptr.
        "Ptr " <> paren (go inner)
      TKUser name _scs -> renderUser aliases name
      TKType _alias inner -> go inner
      TKFunction ret params -> renderFunPtr aliases ret params
      TKInlineStruct _ -> "{- inline struct unsupported -}"
      TKInlineUnion _ -> "{- inline union unsupported -}"

renderPointer :: TypeAliasMap -> TypeKind -> Text
renderPointer aliases inner = case inner of
  -- void* → Ptr ()
  TKBuiltin "void" _ -> "Ptr ()"
  -- const char* → CString. Plain (mutable) char* stays as Ptr CChar
  -- so the caller can tell read-only-input from output-buffer at the
  -- type level (the latter being marshalled later).
  TKBuiltin "char" (Just scs)
    | "const" `elem` scs -> "CString"
  -- Pointer-to-function = FunPtr; the C function-pointer typedef
  -- @void (*Foo)(int)@ parses as TKPointer (TKFunction …), but the
  -- Haskell representation already is FunPtr, so we don't wrap it
  -- in another Ptr layer.
  TKFunction ret params -> renderFunPtr aliases ret params
  _ -> "Ptr " <> paren (render aliases inner)

renderFunPtr :: TypeAliasMap -> TypeKind -> [TypeKind] -> Text
renderFunPtr aliases ret params =
  "FunPtr (" <> body <> ")"
  where
    body = case params of
      [] -> "IO " <> paren (render aliases ret)
      _ -> arrowChain (map paramType params) <> " -> IO " <> paren (render aliases ret)
    -- Function pointer parameters arrive as TKType wrappers; descend
    -- into the inner so we don't print the typedef name as-if-a-type.
    paramType :: TypeKind -> Text
    paramType (TKType _ inner) = render aliases inner
    paramType other = render aliases other
    arrowChain :: [Text] -> Text
    arrowChain = Text.intercalate " -> " . map paren

{- | Map a builtin C type to its 'Foreign.C.Types' / 'Data.Int' /
'Data.Word' / 'Data.ByteString' counterpart.

Width-suffixed ints come through unchanged; other widths fold into
'CInt' & friends. Unknown names emit a literal pass-through plus a
comment marker so a downstream type-check failure is easy to grep
for.
-}

{- | Some C library types arrive as 'TKUser' (because dear_bindings
treats them as platform typedefs rather than primitives) but really
need a 'Foreign.C.Types' counterpart, since the catalog won't carry
a 'Typedef' for them. Apply the user-provided alias map first; fall
back to a built-in shortlist for stdint widths; otherwise pass
through verbatim.
-}
renderUser :: TypeAliasMap -> Text -> Text
renderUser aliases name = case Map.lookup name aliases of
  Just (_module, hsName) -> hsName
  Nothing -> case name of
    "size_t" -> "CSize"
    "ssize_t" -> "CSSize"
    "ptrdiff_t" -> "CPtrdiff"
    "intptr_t" -> "CIntPtr"
    "uintptr_t" -> "CUIntPtr"
    "int8_t" -> "Int8"
    "int16_t" -> "Int16"
    "int32_t" -> "Int32"
    "int64_t" -> "Int64"
    "uint8_t" -> "Word8"
    "uint16_t" -> "Word16"
    "uint32_t" -> "Word32"
    "uint64_t" -> "Word64"
    other -> other

{- | Map a dear-bindings @builtin_type@ name to its Haskell counterpart.
The JSON uses underscores rather than spaces to keep the names
single-token (so @unsigned long long@ shows up as @unsigned_long_long@).
We accept both forms — and the C @stdint.h@ aliases — for resilience
to dear_bindings format tweaks.
-}
renderBuiltin :: Text -> Text
renderBuiltin t = case Text.replace " " "_" t of
  "void" -> "()"
  "bool" -> "CBool"
  "char" -> "CChar"
  "signed_char" -> "CSChar"
  "unsigned_char" -> "CUChar"
  "short" -> "CShort"
  "unsigned_short" -> "CUShort"
  "int" -> "CInt"
  "unsigned_int" -> "CUInt"
  "long" -> "CLong"
  "unsigned_long" -> "CULong"
  "long_long" -> "CLLong"
  "unsigned_long_long" -> "CULLong"
  "float" -> "CFloat"
  "double" -> "CDouble"
  "size_t" -> "CSize"
  "ptrdiff_t" -> "CPtrdiff"
  "int8_t" -> "Int8"
  "int16_t" -> "Int16"
  "int32_t" -> "Int32"
  "int64_t" -> "Int64"
  "uint8_t" -> "Word8"
  "uint16_t" -> "Word16"
  "uint32_t" -> "Word32"
  "uint64_t" -> "Word64"
  "intptr_t" -> "CIntPtr"
  "uintptr_t" -> "CUIntPtr"
  _ -> t <> "{-?-}"

{- | Wrap a rendering in parens unless it's already a single token.
Used by callers that splice the result into a larger type (Ptr,
FunPtr arrow chains).
-}
paren :: Text -> Text
paren t
  | needsParen t = "(" <> t <> ")"
  | otherwise = t
  where
    needsParen s =
      Text.any (== ' ') s
        && not (Text.isPrefixOf "(" s && Text.isSuffixOf ")" s)

{- | True iff the tree contains an inline struct or union somewhere.
Used by 'FFI.Skip' to drop functions whose signatures embed
anonymous aggregates that v0 has no way to surface.
-}
typeKindContainsInlineAggregate :: TypeKind -> Bool
typeKindContainsInlineAggregate = \case
  TKInlineStruct _ -> True
  TKInlineUnion _ -> True
  TKBuiltin _ _ -> False
  TKUser _ _ -> False
  TKPointer inner _ _ _ -> typeKindContainsInlineAggregate inner
  TKArray inner _ -> typeKindContainsInlineAggregate inner
  TKType _ inner -> typeKindContainsInlineAggregate inner
  TKFunction ret params ->
    typeKindContainsInlineAggregate ret
      || any typeKindContainsInlineAggregate params

{- | Collect every TKUser name reachable from a 'TypeKind' tree. Used
by the runner to decide which alias-map modules a generated module
needs to import.
-}
typeKindUserNames :: TypeKind -> Set Text
typeKindUserNames = \case
  TKBuiltin _ _ -> Set.empty
  TKUser n _ -> Set.singleton n
  TKPointer inner _ _ _ -> typeKindUserNames inner
  TKArray inner _ -> typeKindUserNames inner
  TKType _ inner -> typeKindUserNames inner
  TKFunction ret params ->
    Set.union (typeKindUserNames ret) (Set.unions (map typeKindUserNames params))
  TKInlineStruct _ -> Set.empty
  TKInlineUnion _ -> Set.empty
