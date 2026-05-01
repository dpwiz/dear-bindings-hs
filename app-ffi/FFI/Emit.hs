{-| Render an 'EmitGroup' to the textual contents of a single @.hsc@
file. Skipped entities are folded out and contribute to the
returned 'SkipCounters'.
-}
module FFI.Emit
  ( EmitOptions (..)
  , emitGroup
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Argument (..)
  , Define (..)
  , EnumElement (..)
  , Enum_ (..)
  , Function (..)
  , Struct (..)
  , StructField (..)
  , TypeRef (..)
  , Typedef (..)
  )
import DearBindings.JSON.Types qualified
import FFI.HType (TypeAliasMap, renderArgType, renderHType, renderReturnType, toHsTypeName)
import FFI.Skip
  ( SkipCounters
  , bump
  , conditionalsActiveDefault
  , skipDefine
  , skipEnum
  , skipFunction
  , skipStruct
  , skipTypedef
  )
import FFI.Skip qualified as Skip
import FFI.Slicing (EmitGroup (..))
import FFI.Wrapper (WrapperDef (..), needsWrap, renderWrapper)

data EmitOptions = EmitOptions
  { moduleName :: Text
  -- ^ Fully-qualified Haskell module name (e.g. @DearImGui.Raw.ImGui@).
  , headerInclude :: Text
  {- ^ The @\#include@ literal placed in every emitted @.hsc@ and used
  as the foreign-import header reference. Default is
  @dcimgui_nodefaultargfunctions.h@; the CLI lets the user override.
  -}
  , typesModule :: Maybe Text
  {- ^ Fully-qualified name of the shared types module (e.g.
  @DearImGui.Raw.Types@). Function-only modules import it
  unqualified so type references resolve. 'Nothing' for the types
  module itself.
  -}
  , opaqueStructs :: Set Text
  {- ^ Catalog struct names that we'll render as opaque @data X@. Used
  by the function skip rule to detect by-value struct refs in
  signatures (which can't be marshalled across the @capi@ boundary
  when the Haskell side is an opaque @data X@). Whiteboxed structs
  are excluded from this set — they DO marshal by value.
  -}
  , whiteboxStructs :: Set Text
  {- ^ Catalog struct names rendered as Haskell records with full
  'Storable' instances (peek/poke per field). 'renderStruct' uses
  this to branch between record and opaque emission.
  -}
  , externalTypesModules :: [Text]
  {- ^ Modules from which externally-defined types are imported. In
  impl mode this typically includes the core's @Types@ module
  (e.g. @DearImGui.Raw.Types@) and any third-party-binding
  modules pulled in by 'typeAliases' (e.g. @Vulkan.Core10@).
  Function modules emit @import \<m\>@ unqualified for each.
  Empty for core packages.
  -}
  , externalTypesHidings :: [Text]
  {- ^ Haskell names that the local package re-declares (typically
  full struct definitions for entities the core forward-declared).
  Each external Types import is rendered with a @hiding (n1, n2, …)@
  clause naming these so the local declaration shadows the
  external one. Empty list = plain @import \<m\>@.
  -}
  , typeAliases :: TypeAliasMap
  {- ^ Rename map for TKUser names that resolve via a third-party
  Haskell binding rather than a local declaration. See 'TypeAliasMap'.
  -}
  }
  deriving (Eq, Show)

{- | Render an 'EmitGroup' as the contents of one @.hsc@ file. Returns
the file body, a 'SkipCounters' tally of entities dropped, and the
list of 'WrapperDef's the caller must aggregate (one per function
whose signature touches a whitebox struct by value).
-}
emitGroup :: EmitOptions -> EmitGroup -> (Text, SkipCounters, [WrapperDef])
emitGroup opts g =
  let
    -- dear_bindings sometimes emits both a typedef and an enum for the
    -- same name (e.g. @ImGuiKey@ has @typedef int ImGuiKey@ alongside
    -- @enum ImGuiKey { … }@). Both would render to the same
    -- @type ImGuiKey = CInt@, which Haskell rejects as a duplicate
    -- declaration. The enum carries the value constants, so we keep
    -- it and drop the typedef.
    enumNames = [(e :: Enum_).name | e <- g.enums]
    typedefs' = [t | t <- g.typedefs, t.name `notElem` enumNames]

    (typedefBody, sk1) = renderEach (renderTypedef opts) skipTypedef typedefs' Skip.empty
    (structBody, sk2) = renderEach (renderStruct opts) skipStruct g.structs sk1
    (enumBody, sk3) = renderEach renderEnum skipEnum g.enums sk2
    (defineBody, sk4) = renderEach renderDefine skipDefine g.defines sk3
    (fnBody, sk5, wrappers) = emitFunctions opts g.functions sk4

    body =
      header opts
        <> sectionMaybe "-- typedefs" typedefBody
        <> sectionMaybe "-- structs (opaque)" structBody
        <> sectionMaybe "-- enums" enumBody
        <> sectionMaybe "-- defines" defineBody
        <> sectionMaybe "-- functions" fnBody
  in
    (body, sk5, wrappers)

{- | Function-emission fold. For each function:

* If the function trips a skip rule, count it and drop.
* Otherwise, if its signature touches a whitebox struct by value
  ('FFI.Wrapper.needsWrap'), render the by-pointer 'foreign import'
  + user-facing Haskell wrapper, and accumulate the matching
  'WrapperDef' for later @.cpp@ / @.h@ emission.
* Otherwise, render a direct 'foreign import' to the original C
  function.
-}
emitFunctions
  :: EmitOptions
  -> [Function]
  -> SkipCounters
  -> (Text, SkipCounters, [WrapperDef])
emitFunctions opts fs sk0 = foldr step ("", sk0, []) fs
  where
    step f (acc, sk, ws) = case skipFunction opts.opaqueStructs f of
      Just reason -> (acc, bump reason sk, ws)
      Nothing
        | needsWrap opts.whiteboxStructs f ->
            let w = renderWrapper opts.typeAliases opts.whiteboxStructs f
            in (w.haskell <> "\n" <> acc, sk, w : ws)
        | otherwise ->
            (renderFunction opts f <> acc, sk, ws)

renderEach
  :: (a -> Text)
  -> (a -> Maybe Skip.SkipReason)
  -> [a]
  -> SkipCounters
  -> (Text, SkipCounters)
renderEach r skipFn xs sk0 = foldr step ("", sk0) xs
  where
    step x (acc, sk) = case skipFn x of
      Just reason -> (acc, bump reason sk)
      Nothing -> (r x <> acc, sk)

sectionMaybe :: Text -> Text -> Text
sectionMaybe _ "" = ""
sectionMaybe label body = "\n" <> label <> "\n\n" <> body

-- ---------------------------------------------------------------------------
-- Module header

header :: EmitOptions -> Text
header opts =
  Text.unlines $
    [ "{-# LANGUAGE CApiFFI #-}"
    , "{-# LANGUAGE PatternSynonyms #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , ""
    , "-- This file was generated by dear-bindings-ffi. Do not edit by hand."
    , "-- Haskell function names: lower the first character of the C name."
    , "-- e.g. ImGui_Begin -> imGui_Begin"
    , ""
    , "module " <> opts.moduleName <> " where"
    , ""
    , "import Foreign.C.String (CString)"
    , "import Foreign.C.Types"
    , "import Foreign.Marshal.Alloc (alloca)"
    , "import Foreign.Marshal.Utils (with)"
    , "import Foreign.Ptr (FunPtr, Ptr)"
    , "import Foreign.Storable (Storable (..))"
    , "import Data.Int (Int8, Int16, Int32, Int64)"
    , "import Data.Word (Word8, Word16, Word32, Word64)"
    ]
      <> map renderExternalImport opts.externalTypesModules
      <> typesImportLine opts.typesModule
      <> [ ""
         , "#include \"" <> opts.headerInclude <> "\""
         ]
  where
    typesImportLine Nothing = []
    typesImportLine (Just m) = ["import " <> m]
    renderExternalImport m = case opts.externalTypesHidings of
      [] -> "import " <> m
      hs -> "import " <> m <> " hiding (" <> Text.intercalate ", " hs <> ")"

-- ---------------------------------------------------------------------------
-- Per-entity renderers

renderTypedef :: EmitOptions -> Typedef -> Text
renderTypedef opts t =
  -- Apply toHsTypeName to the typedef's own name too — typedefs from
  -- C may start lowercase (e.g. @stbrp_node_im@), which Haskell rejects
  -- as a type-constructor head.
  "type " <> toHsTypeName t.name <> " = " <> renderHType opts.typeAliases t.type_.description <> "\n"

renderStruct :: EmitOptions -> Struct -> Text
renderStruct opts s
  | s.forwardDeclaration =
      kindComment s <> "data " <> n <> "\n"
  | s.name `Set.member` opts.whiteboxStructs =
      renderWhiteboxStruct opts s
  | otherwise =
      -- #{size} / #{alignment} need the flat C identifier
      -- (Struct.name) rather than the C++ qualified form
      -- (originalFullyQualifiedName) — the latter contains @::@ and
      -- @<>@ for templated types, which hsc2hs would reject. The
      -- Haskell-side name @n@ is the C tag with any leading
      -- underscores stripped (e.g. SDL2 @_SDL_GameController@ →
      -- @SDL_GameController@); the hsc2hs splices keep @s.name@.
      Text.unlines
        [ kindComment s <> "data " <> n
        , "instance Storable " <> n <> " where"
        , "  sizeOf _    = #{size " <> s.name <> "}"
        , "  alignment _ = #{alignment " <> s.name <> "}"
        , "  peek _ = error \"" <> n <> ": opaque struct, peek not supported in v0\""
        , "  poke _ _ = error \"" <> n <> ": opaque struct, poke not supported in v0\""
        ]
  where
    n = toHsTypeName s.name

{- | Whitebox emission: a Haskell record + a fully-implemented
'Storable' instance using hsc2hs @\#{peek}@ / @\#{poke}@ directives
per field. Lets the struct cross the @capi@ boundary by value.

The record is constructed with one record field per C field. Field
names are lowercased on the first character so they're legal Haskell
record fields (the @HasField@ instance auto-derived under
'NoFieldSelectors' uses this lowercased name; the original C name
is preserved in the @\#{peek}@ / @\#{poke}@ directives so layout
matches).

A @{\-\# CTYPE \"<header>\" \"<C name>\" \#-\}@ pragma is attached to
the @data@ declaration so @capi@ knows the corresponding C struct
type. Without this annotation GHC rejects the type as
\"unmarshallable\" when it appears bare (not under @Ptr@) in a
foreign import.

Field access uses 'OverloadedRecordDot' (@v.fieldName@) — already
in @default-extensions@ for both the generator and the consumer
projects, and works under @NoFieldSelectors@ via the @HasField@
typeclass.
-}
renderWhiteboxStruct :: EmitOptions -> Struct -> Text
renderWhiteboxStruct opts s =
  let
    -- @cN@ is the C-side tag (kept verbatim for the CTYPE pragma and
    -- hsc2hs splices); @n@ is the Haskell-side type/constructor name
    -- (leading underscores stripped). They diverge for SDL-style
    -- @_Foo@ tags; otherwise they're identical.
    cN = s.name
    n = toHsTypeName s.name
    fields = s.fields
    recordLines = case fields of
      [] -> []
      (f0 : rest) ->
        ("  { " <> renderRecordField f0)
          : map (\f -> "  , " <> renderRecordField f) rest
            <> ["  }"]
    peekExpr = case fields of
      [] -> "pure " <> n
      (f0 : rest) ->
        n
          <> " <$> "
          <> peekField f0
          <> Text.concat [" <*> " <> peekField f | f <- rest]
    pokeStmts = case fields of
      [] -> ["    pure ()"]
      _ -> map (("    " <>) . pokeField) fields
    ctype =
      "{-# CTYPE \""
        <> opts.headerInclude
        <> "\" \""
        <> cN
        <> "\" #-}"
  in
    Text.unlines $
      [kindComment s <> "data " <> ctype <> " " <> n <> " = " <> n]
        <> recordLines
        <> [ "  deriving (Eq, Show)"
           , ""
           , "instance Storable " <> n <> " where"
           , "  sizeOf _    = #{size " <> cN <> "}"
           , "  alignment _ = #{alignment " <> cN <> "}"
           , "  peek p = " <> peekExpr
           , "  poke p v = do"
           ]
        <> pokeStmts
  where
    renderRecordField f =
      fieldHaskellName f <> " :: " <> renderHType opts.typeAliases f.type_.description
    peekField f =
      "#{peek " <> s.name <> ", " <> f.name <> "} p"
    pokeField f =
      "#{poke "
        <> s.name
        <> ", "
        <> f.name
        <> "} p v."
        <> fieldHaskellName f

{- | Lowercase the first character of a struct-field name so it's a
legal Haskell record selector. ImColor's @Value@ becomes @value@;
already-lowercase or underscore-prefixed names pass through.
-}
fieldHaskellName :: StructField -> Text
fieldHaskellName f = lowerFirst f.name

kindComment :: Struct -> Text
kindComment s =
  let parts =
        [s.kind | s.kind /= "struct"]
          <> ["by-value" | s.byValue]
          <> ["anonymous" | s.isAnonymous]
  in if null parts
       then ""
       else "-- " <> Text.intercalate ", " parts <> "\n"

renderEnum :: Enum_ -> Text
renderEnum e =
  "type "
    <> e.name
    <> " = CInt\n"
    <> Text.concat (map (renderEnumElement e.name) (filter active e.elements))
  where
    -- Drop elements gated by inactive #ifdefs (e.g.
    -- IMGUI_ENABLE_TEST_ENGINE-only flag values). hsc2hs's #const
    -- lookup would otherwise fail to find the C identifier at
    -- preprocessor time. Skip-counter bookkeeping happens at the
    -- enum level, not per-element, so this is a silent filter.
    active el = conditionalsActiveDefault el.conditionals

renderEnumElement :: Text -> EnumElement -> Text
renderEnumElement enumName el
  | isAcceptableConstructorName el.name =
      Text.unlines
        [ "pattern " <> el.name <> " :: " <> enumName
        , "pattern " <> el.name <> " = #{const " <> el.name <> "}"
        ]
  | otherwise = ""

isAcceptableConstructorName :: Text -> Bool
isAcceptableConstructorName n = case Text.uncons n of
  Just (c, _) -> c >= 'A' && c <= 'Z'
  Nothing -> False

renderDefine :: Define -> Text
renderDefine d =
  Text.unlines
    [ "pattern " <> d.name <> " :: CInt"
    , "pattern " <> d.name <> " = #{const " <> d.name <> "}"
    ]

renderFunction :: EmitOptions -> Function -> Text
renderFunction opts f =
  let
    argTypes = map (renderArgType opts.typeAliases) f.arguments
    retType = renderReturnType opts.typeAliases f.returnType
    typeChain'
      | null argTypes = "IO " <> paren retType
      | otherwise =
          Text.intercalate " -> " (map paren argTypes <> ["IO " <> paren retType])
    haskellName = lowerFirst f.name
    cArgs = Text.intercalate ", " (map argDecl f.arguments)
  in
    Text.unlines
      [ "-- C: "
          <> f.returnType.declaration
          <> " "
          <> f.originalFullyQualifiedName
          <> "("
          <> cArgs
          <> ")"
      , "foreign import capi unsafe \""
          <> opts.headerInclude
          <> " "
          <> f.name
          <> "\""
      , "  " <> haskellName <> " :: " <> typeChain'
      ]

argDecl :: Argument -> Text
argDecl a
  | a.isVarargs = "..."
  | otherwise = case a.type_ of
      Just tr ->
        let nm = case a.name of
              Just n -> " " <> n
              Nothing -> ""
        in tr.declaration <> nm
      Nothing -> "?"

lowerFirst :: Text -> Text
lowerFirst t = case Text.uncons t of
  Just (c, rest) -> Text.cons (toLowerChar c) rest
  Nothing -> t
  where
    toLowerChar c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

paren :: Text -> Text
paren t
  | Text.any (== ' ') t && not (Text.isPrefixOf "(" t && Text.isSuffixOf ")" t) =
      "(" <> t <> ")"
  | otherwise = t
