{-| Top-level orchestrator for the FFI generator. Reads one
dear_bindings JSON file, routes every entity into a destination
module, writes one @.hsc@ file per group, and prints a coverage +
skip summary to stderr.
-}
module FFI.Run
  ( RunOptions (..)
  , run
  ) where

import Control.Monad (unless)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import DearBindings.Catalog (Catalog (..))
import DearBindings.Catalog qualified as Catalog
import DearBindings.JSON
  ( Argument (..)
  , Function (..)
  , Struct (..)
  , StructField (..)
  , TypeRef (..)
  , Typedef (..)
  )
import DearBindings.JSON.IO qualified as JSON
import DearBindings.JSON.Types qualified
import FFI.Emit (EmitOptions (..), emitGroup)
import FFI.HType (TypeAliasMap, typeKindUserNames)
import FFI.Module (qualifierToModule, qualifierToPath)
import FFI.Skip (SkipCounters)
import FFI.Skip qualified as Skip
import FFI.Slicing
  ( EmitGroup (..)
  , RouteOptions (..)
  , groupTotal
  , routeEverything
  , typesQualifier
  )
import FFI.Whitebox (whiteboxSet)
import FFI.Wrapper (WrapperDef (..), wrapperHeaderName)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

data RunOptions = RunOptions
  { input :: FilePath
  , output :: FilePath
  , moduleRoot :: Text
  , headerInclude :: Text
  , externalTypesModules :: [Text]
  {- ^ Modules from which externally-defined types are imported
  unqualified into every function module. Typically includes the
  core's @Types@ module in impl mode, plus any third-party Haskell
  bindings whose names appear in 'typeAliasesJson'. Empty for core
  packages.
  -}
  , externalTypesJson :: [FilePath]
  {- ^ One or more reference catalogs (typically the core's
  @dcimgui_nodefaultargfunctions.json@). The structs+typedefs+
  enums in these files form the @externalNames@ set: names that
  appear here are dropped from the impl's local types module
  (already provided via 'externalTypesModules'). Names absent from
  here are impl-owned and stay local.
  -}
  , typeAliasesJson :: Maybe FilePath
  {- ^ Optional path to a JSON map of @{ "VkDevice": { "module":
  "Vulkan.Core10", "name": "Device" } }@ entries. When present,
  TKUser names matching a key are renamed to the mapped Haskell
  name and an @import@ for the providing module is added to every
  generated module.
  -}
  }
  deriving (Eq, Show)

run :: RunOptions -> IO ()
run opts = do
  hdr <- JSON.decodeFile opts.input
  externalNames <- loadExternalNames opts.externalTypesJson
  typeAliases <- loadTypeAliases opts.typeAliasesJson
  let
    catalog = Catalog.fromHeader hdr
    catalogLocalNames =
      Set.unions
        [ Set.fromList (Map.keys catalog.structs)
        , Set.fromList (Map.keys catalog.typedefs)
        , Set.fromList (Map.keys catalog.enums)
        ]
    -- Catalog-reachable TKUser names that aren't resolvable through
    -- the local catalog, an external types module, the alias map, or
    -- the renderUser builtin shortlist. Each becomes a synthetic
    -- opaque @data X@ in the local types group; the by-value skip
    -- rule treats them as un-marshallable too.
    --
    -- Names with a non-Haskell-typecon shape (e.g. @va_list@) are
    -- filtered out — the skip rules already drop entities that touch
    -- them; synthesizing @data va_list@ would be a parse error.
    unmappedExt =
      Set.filter validHaskellTypeCon $
        catalogReferencedTKUserNames catalog
          `Set.difference` Set.unions
            [ catalogLocalNames
            , externalNames
            , Map.keysSet typeAliases
            , builtinUserNames
            ]
    allStructs = Set.fromList (Map.keys catalog.structs)
    whiteboxed = whiteboxSet catalog
    -- Skip rule's by-value-blocked set is the catalog structs we
    -- haven't whiteboxed plus any unmapped externals: each emits as
    -- an opaque @data X@, which @capi@ can't marshal by value. Names
    -- routed through the alias map (e.g. handles → @Ptr X_T@) DO
    -- marshal — they aren't in this set.
    opaqueStructs =
      Set.difference allStructs whiteboxed
        `Set.union` unmappedExt
    routeOpts =
      RouteOptions
        { externalNames = externalNames
        , unmappedExternalNames = unmappedExt
        }
    groups = routeEverything routeOpts catalog
    routedTotal = sum (map groupTotal groups)
    -- Distinct alias modules are imported by every generated module
    -- (function modules and the local Types module). Cheaper to
    -- declare them all than to scan per-group for which ones are
    -- actually used; -Wno-unused-imports already suppresses the
    -- noise. Empty for impls that don't use the alias mechanism.
    aliasModules =
      Set.toAscList (Set.fromList [m | (m, _) <- Map.elems typeAliases])
    -- Forward-declared structs and typedefs whose names match the
    -- external set are intentionally dropped by the router (they
    -- live in the external module). Synthetic opaques get added on
    -- top of the catalog's own count.
    droppedFwdDecls =
      length [s | s <- Map.elems catalog.structs, s.forwardDeclaration, Set.member s.name externalNames]
    droppedTypedefs =
      length [t | t <- Map.elems catalog.typedefs, Set.member t.name externalNames]
    catalogTotal =
      Catalog.size catalog - droppedFwdDecls - droppedTypedefs + Set.size unmappedExt

  -- Coverage assertion. The FFI generator must never silently drop a
  -- C symbol — every entity in the parsed catalog has to land in some
  -- emit group, even if a later skip rule removes it from output.
  unless (routedTotal == catalogTotal) $
    error $
      "FFI.Run: coverage mismatch — adjusted catalog has "
        <> show catalogTotal
        <> " entities but routing placed "
        <> show routedTotal
        <> ". This is a bug in FFI.Slicing.routeEverything."

  createDirectoryIfMissing True opts.output

  let hasLocalTypes = any (\g -> g.qualifier == typesQualifier) groups

  (finalCounters, allWrappers) <-
    foldl'
      (step opaqueStructs whiteboxed hasLocalTypes typeAliases aliasModules)
      (pure (Skip.empty, []))
      groups

  writeWrapperFiles opts allWrappers

  hPutStrLn stderr $
    "Generated "
      <> show (length groups)
      <> " .hsc files and "
      <> show (length allWrappers)
      <> " by-value wrappers under "
      <> opts.output
      <> "."
  TextIO.hPutStrLn stderr (Skip.renderReport finalCounters)
  where
    step opaque whitebox hasLocalTypes aliases aliasMods acc g = do
      (sk, ws) <- acc
      (sk', ws') <- writeOne opts opaque whitebox hasLocalTypes aliases aliasMods g sk
      pure (sk', ws <> ws')

writeOne
  :: RunOptions
  -> Set Text
  -> Set Text
  -> Bool
  -> TypeAliasMap
  -> [Text]
  -> EmitGroup
  -> SkipCounters
  -> IO (SkipCounters, [WrapperDef])
writeOne opts opaque whitebox hasLocalTypes aliases aliasMods g priorSk = do
  let
    moduleName = qualifierToModule opts.moduleRoot g.qualifier
    typesModuleName = qualifierToModule opts.moduleRoot typesQualifier
    -- The local Types module is suppressed both for the Types group
    -- itself (it doesn't import itself) and when the impl mode has
    -- no local types group at all.
    localTypesImport
      | g.qualifier == typesQualifier = Nothing
      | hasLocalTypes = Just typesModuleName
      | otherwise = Nothing
    -- Function modules import the external Types modules (e.g. core's
    -- Types and any third-party binding modules from typeAliases);
    -- the Types group imports only the third-party modules, since
    -- its own structs may reference aliased SDK types but the core
    -- Types module is its own peer. -Wno-unused-imports keeps the
    -- noise down when no aliases are actually used.
    externalImports
      | g.qualifier == typesQualifier = aliasMods
      | otherwise = opts.externalTypesModules <> aliasMods
    eopts =
      EmitOptions
        { moduleName = moduleName
        , headerInclude = opts.headerInclude
        , typesModule = localTypesImport
        , externalTypesModules = externalImports
        , opaqueStructs = opaque
        , whiteboxStructs = whitebox
        , typeAliases = aliases
        }
    (body, deltaSk, wrappers) = emitGroup eopts g
    -- Haskell modules land under the package's @src/@ subdir (the
    -- standard @source-dirs@ for stack/cabal). Wrappers go to
    -- @cbits/@. Together they fill the package layout the consumer
    -- expects.
    path = qualifierToPath (opts.output </> "src") opts.moduleRoot g.qualifier
  createDirectoryIfMissing True (takeDirectory path)
  TextIO.writeFile path body
  pure (mergeCounters priorSk deltaSk, wrappers)

{- | Write the single shared @DearImGuiWrappers.{cpp,h}@ pair holding
all by-value-shim definitions. The header is included by both the
generated @.cpp@ and (transitively, via Haskell @capi@) by every
generated @.hsc@ that has wrapped functions.
-}
writeWrapperFiles :: RunOptions -> [WrapperDef] -> IO ()
writeWrapperFiles opts ws = do
  let
    cbitsDir = opts.output </> "cbits"
    hPath = cbitsDir </> Text.unpack wrapperHeaderName
    cPath = cbitsDir </> "DearImGuiWrappers.cpp"
    decls = Text.concat [w.cDecl <> "\n" | w <- ws]
    bodies = Text.concat [w.cBody <> "\n" | w <- ws]
    hText =
      Text.unlines
        [ "// Generated by dear-bindings-ffi. Do not edit."
        , "#ifndef DEAR_IMGUI_WRAPPERS_H"
        , "#define DEAR_IMGUI_WRAPPERS_H"
        , ""
        , "#include \"" <> opts.headerInclude <> "\""
        , ""
        , "#ifdef __cplusplus"
        , "extern \"C\" {"
        , "#endif"
        , ""
        ]
        <> decls
        <> Text.unlines
          [ ""
          , "#ifdef __cplusplus"
          , "}"
          , "#endif"
          , ""
          , "#endif"
          ]
    cText =
      Text.unlines
        [ "// Generated by dear-bindings-ffi. Do not edit."
        , "#include \"" <> wrapperHeaderName <> "\""
        , ""
        ]
        <> bodies
  createDirectoryIfMissing True cbitsDir
  TextIO.writeFile hPath hText
  TextIO.writeFile cPath cText

{- | Load the union of struct/typedef/enum names defined in each
reference catalog. These are the names the impl can rely on the
external module to provide — the router drops matching forward
decls and typedefs from the impl's local types group.
-}
loadExternalNames :: [FilePath] -> IO (Set Text)
loadExternalNames paths = do
  cats <- traverse (fmap Catalog.fromHeader . JSON.decodeFile) paths
  pure $
    Set.unions
      [ Set.unions
          [ Set.fromList (Map.keys c.structs)
          , Set.fromList (Map.keys c.typedefs)
          , Set.fromList (Map.keys c.enums)
          ]
      | c <- cats
      ]

{- | Decode a type-aliases JSON of the form

> { "VkDevice": { "module": "Vulkan.Core10", "name": "Device" },
>   "VkResult": { "module": "Vulkan.Core10", "name": "Result" }, ... }

into a 'TypeAliasMap'. 'Nothing' produces an empty map. Errors are
fatal — a typo'd alias would silently emit a wrong type otherwise.
-}
loadTypeAliases :: Maybe FilePath -> IO TypeAliasMap
loadTypeAliases Nothing = pure Map.empty
loadTypeAliases (Just p) = do
  bs <- BSL.readFile p
  case Aeson.eitherDecode bs :: Either String (Map Text TypeAliasEntry) of
    Left e -> error $ "type-aliases-json (" <> p <> "): " <> e
    Right m -> pure $ Map.map (\e -> (e.module_, e.name)) m

data TypeAliasEntry = TypeAliasEntry
  { module_ :: Text
  , name :: Text
  }
  deriving (Eq, Show)

instance Aeson.FromJSON TypeAliasEntry where
  parseJSON = Aeson.withObject "TypeAliasEntry" $ \o -> do
    m <- o .: "module"
    n <- o .: "name"
    pure TypeAliasEntry{module_ = m, name = n}

{- | Walk every kept catalog function and struct collecting the TKUser
type names referenced. Used (post-difference against locals,
externals, aliases, and builtins) to decide which names need
synthetic opaque @data X@ declarations in the local types module.
-}
catalogReferencedTKUserNames :: Catalog -> Set Text
catalogReferencedTKUserNames c =
  Set.unions $
    [typeKindUserNames f.returnType.description | f <- Map.elems c.functions]
      <> [typeKindUserNames a.tr.description | f <- Map.elems c.functions, a <- argTypeRefs f.arguments]
      <> [ typeKindUserNames sf.type_.description
         | s <- Map.elems c.structs
         , not s.forwardDeclaration
         , sf <- s.fields
         ]
      <> [typeKindUserNames t.type_.description | t <- Map.elems c.typedefs]
  where
    argTypeRefs :: [Argument] -> [TaggedTypeRef]
    argTypeRefs xs = [TaggedTypeRef{tr = r} | a <- xs, Just r <- [a.type_]]

newtype TaggedTypeRef = TaggedTypeRef {tr :: TypeRef}

{- | Names that 'FFI.HType.renderUser' rewrites to a Foreign.C.Types or
Data.Word/Data.Int counterpart without consulting the alias map. They
DON'T need synthetic opaques and they aren't subject to by-value-skip.
-}
builtinUserNames :: Set Text
builtinUserNames =
  Set.fromList
    [ "size_t"
    , "ssize_t"
    , "ptrdiff_t"
    , "intptr_t"
    , "uintptr_t"
    , "int8_t"
    , "int16_t"
    , "int32_t"
    , "int64_t"
    , "uint8_t"
    , "uint16_t"
    , "uint32_t"
    , "uint64_t"
    ]

{- | True iff the name can be the head of a Haskell @data@ declaration:
starts uppercase and has no spaces or punctuation other than @_@.
Filters out @va_list@ and similar lowercased / underscored
platform names that the skip rules already handle.
-}
validHaskellTypeCon :: Text -> Bool
validHaskellTypeCon n = case Text.uncons n of
  Just (c, rest)
    | c >= 'A' && c <= 'Z' -> Text.all isOk rest
  _ -> False
  where
    isOk ch =
      (ch >= 'A' && ch <= 'Z')
        || (ch >= 'a' && ch <= 'z')
        || (ch >= '0' && ch <= '9')
        || ch == '_'

mergeCounters :: SkipCounters -> SkipCounters -> SkipCounters
mergeCounters a b =
  Skip.SkipCounters
    { Skip.varargsFunctions = a.varargsFunctions + b.varargsFunctions
    , Skip.conditionalEntities = a.conditionalEntities + b.conditionalEntities
    , Skip.defaultArgHelpers = a.defaultArgHelpers + b.defaultArgHelpers
    , Skip.imstrOrUnformattedHelpers = a.imstrOrUnformattedHelpers + b.imstrOrUnformattedHelpers
    , Skip.nonNumericDefines = a.nonNumericDefines + b.nonNumericDefines
    , Skip.inlineAggregateInSig = a.inlineAggregateInSig + b.inlineAggregateInSig
    , Skip.anonymousTypes = a.anonymousTypes + b.anonymousTypes
    , Skip.byValueStructInSig = a.byValueStructInSig + b.byValueStructInSig
    }
