{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The @generate@ subcommand: turn a 'Catalog' into a directory tree
of one-document-per-entity pandoc files plus index pages at the root
and inside each category.
-}
module Generate
  ( run
  ) where

import Catalog (Catalog (..))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Render qualified
import Render.Common
  ( Category (..)
  , LinkBase (..)
  , categoryDir
  , categoryHref
  , categoryLabel
  , entityHref
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Builder qualified as B
import Writer (Format (..))
import Writer qualified

{- | Top-level entry point. Renders the entire catalog to @outdir@ in
the chosen format. Existing files are overwritten; missing
directories are created.
-}
run :: Catalog -> LinkBase -> Format -> FilePath -> IO ()
run catalog base format outdir = Pandoc.runIOorExplode $ do
  -- Every page in the tree has exactly one top-level heading, so a
  -- table of contents would be a single-item list — we always omit it.
  opts <- Writer.formatOptions format False True
  liftIO $ createDirectoryIfMissing True outdir

  -- Top-level index
  topText <- format.write opts (rootDoc base catalog)
  liftIO $ Text.writeFile (outdir </> Text.unpack ("index." <> base.extension)) topText

  emitCategory
    base
    outdir
    format
    opts
    Defines
    [(n, Render.renderDefine x) | (n, x) <- Map.toAscList catalog.defines]
  emitCategory
    base
    outdir
    format
    opts
    Enums
    [(n, Render.renderEnum x) | (n, x) <- Map.toAscList catalog.enums]
  emitCategory
    base
    outdir
    format
    opts
    Typedefs
    [(n, Render.renderTypedef x) | (n, x) <- Map.toAscList catalog.typedefs]
  emitCategory
    base
    outdir
    format
    opts
    Structs
    [(n, Render.renderStruct x) | (n, x) <- Map.toAscList catalog.structs]
  emitCategory
    base
    outdir
    format
    opts
    Functions
    [(n, Render.renderFunction x) | (n, x) <- Map.toAscList catalog.functions]

emitCategory
  :: LinkBase
  -> FilePath
  -> Format
  -> Pandoc.WriterOptions
  -> Category
  -> [(Text, Blocks)]
  -> Pandoc.PandocIO ()
emitCategory base outdir format opts cat entries = do
  let catDir = outdir </> Text.unpack (categoryDir cat)
  liftIO $ createDirectoryIfMissing True catDir

  -- Category index page
  idxText <- format.write opts (categoryDoc base cat (map fst entries))
  liftIO $
    Text.writeFile
      (catDir </> Text.unpack ("index." <> base.extension))
      idxText

  -- One file per entity
  forM_ entries $ \(name, blocks) -> do
    let
      doc = B.setMeta "pagetitle" (B.text name) (B.doc blocks)
      path = catDir </> Text.unpack (name <> "." <> base.extension)
    text <- format.write opts doc
    liftIO $ Text.writeFile path text

-- ---------------------------------------------------------------------------
-- Index documents

rootDoc :: LinkBase -> Catalog -> Pandoc.Pandoc
rootDoc base catalog =
  B.setMeta "pagetitle" (B.text "dear-imgui API") $
    B.doc $
      B.header 1 (B.text "dear-imgui API")
        <> B.bulletList (map link allCategories)
  where
    link cat =
      B.plain $
        B.link (categoryHref base 0 cat) "" (B.text (categoryLabel cat))
          <> B.space
          <> B.text (countLabel cat)
    countLabel cat = "(" <> Text.pack (show (catalogCount cat catalog)) <> ")"

categoryDoc :: LinkBase -> Category -> [Text] -> Pandoc.Pandoc
categoryDoc base cat names =
  B.setMeta "pagetitle" (B.text (categoryLabel cat)) $
    B.doc $
      B.header 1 (B.text (categoryLabel cat))
        <> case names of
          [] -> B.para (B.emph (B.text "(none)"))
          _ -> mconcat (map renderGroup (groupByQualifier names))
  where
    renderGroup :: (Maybe Text, [(Text, Text)]) -> Blocks
    renderGroup (qual, entries) =
      let listing = B.bulletList (map item entries)
      in case qual of
           Nothing -> listing
           Just q -> B.header 2 (B.code q) <> listing

    -- entries arrive as (full_name, short_name)
    item :: (Text, Text) -> Blocks
    item (full, short) =
      B.plain $ B.link (entityHref base 1 cat full) "" (B.code short)

{- | Split a qualified C name like @ImDrawList_AddCircle@ on the LAST
underscore: @(Just "ImDrawList", "AddCircle")@. Names with no
underscore (@ImVec2@) or a trailing underscore (@ImGuiWindowFlags_@,
the dear-bindings convention for flag-enum tags) are treated as
unqualified — they show up under no header on the index page.
-}
splitQualifier :: Text -> (Maybe Text, Text)
splitQualifier name = case Text.breakOnEnd "_" name of
  ("", _) -> (Nothing, name)
  (_, "") -> (Nothing, name)
  (qual, n) -> (Just (Text.dropEnd 1 qual), n)

{- | Group a category's names by their qualifier. The 'Nothing' bucket
(unqualified names) sorts first; remaining buckets are alphabetical.
Each value list is @(full_name, short_name)@ sorted by short name.
-}
groupByQualifier :: [Text] -> [(Maybe Text, [(Text, Text)])]
groupByQualifier names =
  let
    pairs = [(qual, (full, short)) | full <- names, let (qual, short) = splitQualifier full]
    grouped = Map.fromListWith (++) [(q, [v]) | (q, v) <- pairs]
  in
    [(q, sortOn snd entries) | (q, entries) <- Map.toAscList grouped]

-- ---------------------------------------------------------------------------
-- Helpers

allCategories :: [Category]
allCategories = [minBound .. maxBound]

catalogCount :: Category -> Catalog -> Int
catalogCount c catalog = case c of
  Defines -> Map.size catalog.defines
  Enums -> Map.size catalog.enums
  Typedefs -> Map.size catalog.typedefs
  Structs -> Map.size catalog.structs
  Functions -> Map.size catalog.functions
