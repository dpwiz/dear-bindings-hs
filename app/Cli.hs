{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| optparse-applicative parser for the two subcommands. Lives apart
from 'Main' so the parser can be inspected / re-used from a REPL,
and to keep CLI surface in one searchable place.
-}
module Cli
  ( Command (..)
  , GenerateOptions (..)
  , QueryOptions (..)
  , parser
  , parserInfo
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.Catalog (Filter (..))
import DearBindings.Qualifier (Category (..))
import Options.Applicative
import Writer (Format)
import Writer qualified

data Command
  = Generate GenerateOptions
  | Query QueryOptions

data GenerateOptions = GenerateOptions
  { inputs :: [FilePath]
  , output :: FilePath
  , format :: Format
  , basePath :: Maybe Text
  }

data QueryOptions = QueryOptions
  { inputs :: [FilePath]
  , filter_ :: Filter
  , format :: Format
  }

parserInfo :: ParserInfo Command
parserInfo =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc
          "Render dear-bindings JSON metadata for human consumption \
          \(static catalog tree or one-shot terminal lookup)."
        <> header "dear-bindings-doc — explore the dear-imgui API"
    )

parser :: Parser Command
parser =
  hsubparser
    ( command
        "generate"
        ( info
            (Generate <$> generateOptions)
            (progDesc "Render the whole catalog into a directory tree.")
        )
        <> command
          "query"
          ( info
              (Query <$> queryOptions)
              (progDesc "Print one or more entries to stdout.")
          )
    )

-- ---------------------------------------------------------------------------
-- generate

generateOptions :: Parser GenerateOptions
generateOptions = do
  inputs <- inputsArg
  output <-
    strOption
      ( long "output"
          <> short 'o'
          <> metavar "DIR"
          <> help "Output directory (created if missing)."
      )
  format <- formatOption "html5"
  basePath <-
    optional $
      option
        (eitherReader parseBasePath)
        ( long "base-path"
            <> metavar "PREFIX"
            <> help
              "URL prefix for internal links (e.g. /imgui-docs/). \
              \Default: relative paths."
        )
  pure GenerateOptions{..}

parseBasePath :: String -> Either String Text
parseBasePath s
  | null s = Left "--base-path must be non-empty (omit the flag for relative links)"
  | otherwise = Right $ ensureTrailingSlash (Text.pack s)
  where
    ensureTrailingSlash t
      | "/" `Text.isSuffixOf` t = t
      | otherwise = t <> "/"

-- ---------------------------------------------------------------------------
-- query

queryOptions :: Parser QueryOptions
queryOptions = do
  inputs <- inputsArg
  filter_ <- filterParser
  format <- formatOption "plain"
  pure QueryOptions{..}

filterParser :: Parser Filter
filterParser = do
  cats <- categoriesParser
  names <-
    many $
      strOption
        ( long "name"
            <> metavar "NAME"
            <> help "Exact name match (repeatable)."
        )
  patterns <-
    many $
      strOption
        ( long "match"
            <> metavar "PATTERN"
            <> help "Case-insensitive substring match (repeatable)."
        )
  pure
    Filter
      { categories = cats
      , names = names
      , patterns = patterns
      }

categoriesParser :: Parser (Set.Set Category)
categoriesParser =
  Set.fromList . catMaybes'
    <$> traverse
      cat
      [ (Defines, "define")
      , (Enums, "enum")
      , (Typedefs, "typedef")
      , (Structs, "struct")
      , (Functions, "function")
      ]
  where
    cat (c, name) =
      flag
        Nothing
        (Just c)
        (long name <> help ("Include " <> name <> "s in the result."))
    catMaybes' = foldr (\m acc -> maybe acc (: acc) m) []

-- ---------------------------------------------------------------------------
-- shared

inputsArg :: Parser [FilePath]
inputsArg =
  some $
    strArgument
      ( metavar "INPUT..."
          <> help "Path(s) to dear-bindings JSON file(s)."
      )

formatOption :: Text -> Parser Format
formatOption def =
  option
    (eitherReader parseFmt)
    ( long "to"
        <> short 't'
        <> metavar "FORMAT"
        <> value (fromJustOrDie def)
        <> showDefaultWith (\f -> Text.unpack f.name)
        <> help ("Output format: " <> formatList)
    )
  where
    parseFmt s = case Writer.lookupFormat (Text.pack s) of
      Just f -> Right f
      Nothing ->
        Left $
          "unknown format "
            <> show s
            <> " (known: "
            <> formatList
            <> ")"
    fromJustOrDie n = case Writer.lookupFormat n of
      Just f -> f
      Nothing -> error ("Cli: missing default format " <> Text.unpack n)
    formatList = unwords (map (Text.unpack . (.name)) Writer.formats)
