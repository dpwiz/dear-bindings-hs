{-# LANGUAGE ApplicativeDo #-}

-- | optparse-applicative parser for @dear-bindings-ffi@.
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
  externalTypesModules <-
    many $
      option
        textRead
        ( long "external-types-module"
            <> metavar "MODULE"
            <> help
              "Module from which externally-defined types are imported \
              \unqualified into every generated module. Repeatable. \
              \Typically: --external-types-module DearImGui.Raw.Types \
              \(the core's types) plus any third-party Haskell-binding \
              \modules referenced via --type-aliases-json."
        )
  externalTypesJson <-
    many $
      strOption
        ( long "external-types-json"
            <> metavar "PATH"
            <> help
              "Path to a dear-bindings JSON whose struct/typedef/enum \
              \names should be treated as externally provided. \
              \Repeatable. Forward-declared structs and typedefs whose \
              \names match are dropped from the impl's local types \
              \module (the external module supplies them); names not \
              \found here stay local (impl-owned opaque types or \
              \impl-only typedefs)."
        )
  typeAliasesJson <-
    optional $
      strOption
        ( long "type-aliases-json"
            <> metavar "PATH"
            <> help
              "Path to a JSON map of type renames. Each TKUser name \
              \matched as a key (e.g. VkDevice) is rewritten to the \
              \mapped Haskell name (e.g. Device) and the providing \
              \module is imported. Used to resolve third-party-binding \
              \types like the Haskell `vulkan` package's exports."
        )
  pure
    RunOptions
      { input
      , output
      , moduleRoot
      , headerInclude
      , externalTypesModules
      , externalTypesJson
      , typeAliasesJson
      }

textRead :: ReadM Text
textRead = Text.pack <$> str
