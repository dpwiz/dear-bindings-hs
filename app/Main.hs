{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Catalog qualified
import Cli (Command (..), GenerateOptions (..), QueryOptions (..))
import Cli qualified
import DearBindings.JSON.IO qualified as JSON
import Generate qualified
import Options.Applicative (execParser)
import Query qualified
import Render.Common (LinkBase (LinkBase))
import Writer (Format (..))

main :: IO ()
main = do
  cmd <- execParser Cli.parserInfo
  case cmd of
    Generate GenerateOptions{..} -> do
      headers <- mapM JSON.decodeFile inputs
      let
        Format _ ext _ = format
        catalog = Catalog.fromHeaders headers
        base = LinkBase basePath ext
      Generate.run catalog base format output
    Query QueryOptions{inputs = qInputs, filter_, format = qFormat} -> do
      headers <- mapM JSON.decodeFile qInputs
      let catalog = Catalog.applyFilter filter_ (Catalog.fromHeaders headers)
      Query.run catalog qFormat
