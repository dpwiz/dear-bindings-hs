{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The @query@ subcommand: filter the catalog and dump the chosen
entries to stdout as one concatenated pandoc document. Same
per-record renderers as @generate@; only the wrapping differs.
-}
module Query
  ( run
  ) where

import Catalog (Catalog (..))
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Text
import Render qualified
import Render.Common (emptyContext)
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Builder qualified as B
import Writer (Format (..))
import Writer qualified

run :: Catalog -> Format -> IO ()
run catalog format = Pandoc.runIOorExplode $ do
  opts <- Writer.formatOptions format False False
  text <- format.write opts (B.doc (allBlocks catalog))
  liftIO (Text.putStr text)

{- | Concatenate the filtered entries from every category into one
document body. Order: defines → enums → typedefs → structs →
functions, matching the output directory.
-}
allBlocks :: Catalog -> Blocks
allBlocks c =
  mconcat
    [ mconcat [Render.renderDefine emptyContext x | (_, x) <- Map.toAscList c.defines]
    , mconcat [Render.renderEnum emptyContext x | (_, x) <- Map.toAscList c.enums]
    , mconcat [Render.renderTypedef emptyContext x | (_, x) <- Map.toAscList c.typedefs]
    , mconcat [Render.renderStruct emptyContext x | (_, x) <- Map.toAscList c.structs]
    , mconcat [Render.renderFunction emptyContext x | (_, x) <- Map.toAscList c.functions]
    ]
