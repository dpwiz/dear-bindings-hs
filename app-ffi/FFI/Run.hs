
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
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import DearBindings.Catalog (Catalog (..))
import DearBindings.Catalog qualified as Catalog
import DearBindings.JSON.IO qualified as JSON
import DearBindings.JSON.Types qualified
import FFI.Emit (EmitOptions (..), emitGroup)
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
import Data.Maybe (isJust)
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
  , externalTypesModule :: Maybe Text
  -- ^ When set, switches the generator to "impl mode": types
  -- referenced but not locally defined are imported from this
  -- module instead of redeclared, and forward-declared structs
  -- are skipped. Empty/absent for core packages.
  }
  deriving (Eq, Show)

run :: RunOptions -> IO ()
run opts = do
  hdr <- JSON.decodeFile opts.input
  let
    catalog = Catalog.fromHeader hdr
    allStructs = Set.fromList (Map.keys catalog.structs)
    whiteboxed = whiteboxSet catalog
    -- Skip rule's by-value-blocked set is the catalog structs we
    -- haven't whiteboxed: a function taking one of these by value
    -- can't be marshalled, so we drop it. Whiteboxed structs DO
    -- marshal by value (peek/poke per field).
    opaqueStructs = Set.difference allStructs whiteboxed
    implMode = isJust opts.externalTypesModule
    routeOpts = RouteOptions{implMode = implMode}
    groups = routeEverything routeOpts catalog
    routedTotal = sum (map groupTotal groups)
    -- In impl mode, forward-declared structs are intentionally
    -- dropped (they live in the core's Types module). Discount them
    -- from the catalog total so the coverage check stays meaningful.
    droppedFwdDecls
      | implMode = length [s | s <- Map.elems catalog.structs, s.forwardDeclaration]
      | otherwise = 0
    droppedTypedefs
      | implMode = Map.size catalog.typedefs
      | otherwise = 0
    catalogTotal = Catalog.size catalog - droppedFwdDecls - droppedTypedefs

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
    foldl' (step opaqueStructs whiteboxed hasLocalTypes) (pure (Skip.empty, [])) groups

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
    step opaque whitebox hasLocalTypes acc g = do
      (sk, ws) <- acc
      (sk', ws') <- writeOne opts opaque whitebox hasLocalTypes g sk
      pure (sk', ws <> ws')

writeOne
  :: RunOptions
  -> Set Text
  -> Set Text
  -> Bool
  -> EmitGroup
  -> SkipCounters
  -> IO (SkipCounters, [WrapperDef])
writeOne opts opaque whitebox hasLocalTypes g priorSk = do
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
    -- Function modules import the external Types from the core
    -- package; the Types group itself doesn't (it has no
    -- function-side type references). Suppressing it on the Types
    -- group keeps GHC quiet about unused imports anyway.
    externalImport
      | g.qualifier == typesQualifier = Nothing
      | otherwise = opts.externalTypesModule
    eopts =
      EmitOptions
        { moduleName = moduleName
        , headerInclude = opts.headerInclude
        , typesModule = localTypesImport
        , externalTypesModule = externalImport
        , opaqueStructs = opaque
        , whiteboxStructs = whitebox
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
  let cbitsDir = opts.output </> "cbits"
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
