{-# LANGUAGE ApplicativeDo #-}

{-| optparse-applicative parser for @dear-bindings-ffi@.
-}
module Cli
  ( parserInfo
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import FFI.Run (RunOptions (..))
import Options.Applicative

parserInfo :: ParserInfo RunOptions
parserInfo =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc
          "Generate raw Haskell FFI bindings (.hsc files) from a \
          \dear-bindings JSON catalog. Canonical input is the \
          \_nodefaultargfunctions JSON family."
        <> header "dear-bindings-ffi — generate raw FFI bindings"
    )

parser :: Parser RunOptions
parser = do
  input <-
    strOption
      ( long "input"
          <> short 'i'
          <> metavar "PATH"
          <> help
            "Path to a dear-bindings JSON file. The canonical choice \
            \is dcimgui_nodefaultargfunctions.json (or its _internal \
            \companion); the regular dcimgui.json works too but the \
            \_Ex helper functions add no value at the FFI layer."
      )
  output <-
    strOption
      ( long "output"
          <> short 'o'
          <> metavar "DIR"
          <> help "Output directory (created if missing)."
      )
  moduleRoot <-
    option
      textRead
      ( long "module-root"
          <> metavar "PREFIX"
          <> help
            "Haskell module-name prefix for generated modules \
            \(e.g. DearImGui.Raw)."
      )
  headerInclude <-
    option
      textRead
      ( long "header"
          <> metavar "FILENAME"
          <> value "dcimgui_nodefaultargfunctions.h"
          <> showDefault
          <> help
            "C header filename to #include in every emitted .hsc and \
            \to use as the foreign-import header reference."
      )
  externalTypesModule <-
    optional $
      option
        textRead
        ( long "external-types-module"
            <> metavar "MODULE"
            <> help
              "Switches the generator into impl mode. When set, types \
              \referenced but not locally defined are imported from \
              \this module (e.g. DearImGui.Raw.Types from a sibling \
              \core package), and forward-declared structs are skipped \
              \rather than redeclared."
        )
  pure RunOptions{input, output, moduleRoot, headerInclude, externalTypesModule}

textRead :: ReadM Text
textRead = Text.pack <$> str
