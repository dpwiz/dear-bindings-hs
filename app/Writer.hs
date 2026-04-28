{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Output-format dispatch table. Both subcommands share this so a new
format ('Format' added to 'formats') automatically becomes available
in @generate@ and @query@.
-}
module Writer
  ( Format (..)
  , formats
  , defaultFormat
  , lookupFormat
  , formatOptions
  ) where

import Data.Text (Text)
import Text.Pandoc qualified as Pandoc

data Format = Format
  { name :: Text
  , extension :: Text
  , write :: Pandoc.WriterOptions -> Pandoc.Pandoc -> Pandoc.PandocIO Text
  }

formats :: [Format]
formats =
  [ Format "html5" "html" Pandoc.writeHtml5String
  , Format "html" "html" Pandoc.writeHtml5String
  , Format "markdown" "md" Pandoc.writeMarkdown
  , Format "gfm" "md" Pandoc.writeCommonMark
  , Format "man" "1" Pandoc.writeMan
  , Format "plain" "txt" Pandoc.writePlain
  , Format "native" "native" Pandoc.writeNative
  ]

defaultFormat :: Format
defaultFormat = case formats of
  (f : _) -> f
  [] -> error "Writer: empty formats list"

lookupFormat :: Text -> Maybe Format
lookupFormat n = lookup n [(f.name, f) | f <- formats]

{- | Build 'Pandoc.WriterOptions' for a format. The @standalone@ flag
requests pandoc's default standalone template — needed in
@generate@ so each emitted file is a complete document.
-}
formatOptions
  :: Format
  -> Bool
  -- ^ include table of contents
  -> Bool
  -- ^ standalone (load default template)
  -> Pandoc.PandocIO Pandoc.WriterOptions
formatOptions f toc standalone = do
  template <-
    if standalone
      then Just <$> Pandoc.compileDefaultTemplate f.name
      else pure Nothing
  pure
    Pandoc.def
      { Pandoc.writerTableOfContents = toc
      , Pandoc.writerTemplate = template
      }
