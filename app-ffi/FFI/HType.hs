
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
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Argument (..)
  , TypeKind (..)
  , TypeRef (..)
  )
import DearBindings.JSON.Types qualified

{- | Render a 'TypeKind' tree as a Haskell type. The result is wrapped
in parentheses unless it's a single token, so it can always be
spliced inline (e.g. @\<inner\>@ inside @Ptr (\<inner\>)@).
-}
renderHType :: TypeKind -> Text
renderHType = render

renderReturnType :: TypeRef -> Text
renderReturnType tr = renderHType tr.description

{- | Render an 'Argument''s type as a Haskell type. Varargs ('isVarargs'
true, 'type_' is 'Nothing') don't have a representable Haskell type
and produce a placeholder; callers should filter them out at a
higher level.
-}
renderArgType :: Argument -> Text
renderArgType a = case a.type_ of
  Just tr -> renderHType tr.description
  Nothing -> "{- varargs unsupported -}"

-- ---------------------------------------------------------------------------
-- Internal

render :: TypeKind -> Text
render = \case
  TKBuiltin name _scs -> renderBuiltin name
  TKPointer inner _nullable _ref _scs -> renderPointer inner
  TKArray inner _bounds ->
    -- C arrays decay to pointers in function signatures and at struct
    -- field-by-pointer access; we treat them uniformly as Ptr.
    "Ptr " <> paren (render inner)
  TKUser name _scs -> renderUser name
  TKType _alias inner -> render inner
  TKFunction ret params -> renderFunPtr ret params
  TKInlineStruct _ -> "{- inline struct unsupported -}"
  TKInlineUnion _ -> "{- inline union unsupported -}"

renderPointer :: TypeKind -> Text
renderPointer inner = case inner of
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
  TKFunction ret params -> renderFunPtr ret params
  _ -> "Ptr " <> paren (render inner)

renderFunPtr :: TypeKind -> [TypeKind] -> Text
renderFunPtr ret params =
  "FunPtr (" <> body <> ")"
  where
    body = case params of
      [] -> "IO " <> paren (render ret)
      _ -> arrowChain (map paramType params) <> " -> IO " <> paren (render ret)
    -- Function pointer parameters arrive as TKType wrappers; descend
    -- into the inner so we don't print the typedef name as-if-a-type.
    paramType :: TypeKind -> Text
    paramType (TKType _ inner) = render inner
    paramType other = render other
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
a 'Typedef' for them. Remap a known shortlist; pass everything else
through verbatim.
-}
renderUser :: Text -> Text
renderUser = \case
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

-- | True iff the tree contains an inline struct or union somewhere.
-- Used by 'FFI.Skip' to drop functions whose signatures embed
-- anonymous aggregates that v0 has no way to surface.
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
