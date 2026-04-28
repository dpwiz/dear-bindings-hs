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
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Render qualified
import Render.Common
  ( Category (..)
  , LinkBase (..)
  , LinkContext (..)
  , breadcrumbs
  , categoryDir
  , categoryHref
  , categoryLabel
  , entityHref
  , groupByQualifier
  , rootHref
  , sliceHref
  , slicesDir
  , slicesIndexHref
  )
import Render.Slice qualified
import Slice (Slice (..))
import Slice qualified
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
  -- Plain options: per-entity and category-index pages have a single
  -- top-level heading, so a ToC would be a one-item list.
  --
  -- Toc options: slice pages collect many entities under one document
  -- and benefit from the auto-generated ToC pandoc places at the top.
  plainOpts <- Writer.formatOptions format False True
  tocOpts <- Writer.formatOptions format True True

  liftIO $ createDirectoryIfMissing True outdir

  let
    allSlices = Slice.slices catalog
    -- Single context shared across every renderer call. Per-entity
    -- pages and slice pages all live one directory deep, so depth = 1
    -- works uniformly for relative URLs.
    ctx =
      LinkContext
        { symbols = Slice.buildSymbolTable catalog allSlices
        , base = base
        , depth = 1
        }

  -- Top-level index
  topText <- format.write plainOpts (rootDoc base catalog allSlices)
  liftIO $ Text.writeFile (outdir </> Text.unpack ("index." <> base.extension)) topText

  emitCategory
    base
    outdir
    format
    plainOpts
    Defines
    [(n, Render.renderDefine ctx x) | (n, x) <- Map.toAscList catalog.defines]
  emitCategory
    base
    outdir
    format
    plainOpts
    Enums
    [(n, Render.renderEnum ctx x) | (n, x) <- Map.toAscList catalog.enums]
  emitCategory
    base
    outdir
    format
    plainOpts
    Typedefs
    [(n, Render.renderTypedef ctx x) | (n, x) <- Map.toAscList catalog.typedefs]
  emitCategory
    base
    outdir
    format
    plainOpts
    Structs
    [(n, Render.renderStruct ctx x) | (n, x) <- Map.toAscList catalog.structs]
  emitCategory
    base
    outdir
    format
    plainOpts
    Functions
    [(n, Render.renderFunction ctx x) | (n, x) <- Map.toAscList catalog.functions]

  emitSlices ctx base outdir format plainOpts tocOpts allSlices

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
      crumbs =
        breadcrumbs
          [ (B.text "dear-imgui API", Just (rootHref base 1))
          , (B.text (categoryLabel cat), Just (categoryHref base 1 cat))
          , (B.code name, Nothing)
          ]
      doc =
        B.setMeta "include-before" crumbs $
          B.setMeta "pagetitle" (B.text name) (B.doc blocks)
      path = catDir </> Text.unpack (name <> "." <> base.extension)
    text <- format.write opts doc
    liftIO $ Text.writeFile path text

-- ---------------------------------------------------------------------------
-- Slices

emitSlices
  :: LinkContext
  -> LinkBase
  -> FilePath
  -> Format
  -> Pandoc.WriterOptions
  -> Pandoc.WriterOptions
  -> [Slice]
  -> Pandoc.PandocIO ()
emitSlices ctx base outdir format plainOpts tocOpts ss = do
  let dir = outdir </> Text.unpack slicesDir
  liftIO $ createDirectoryIfMissing True dir

  -- Slices index page
  idxText <- format.write plainOpts (slicesIndexDoc base ss)
  liftIO $
    Text.writeFile
      (dir </> Text.unpack ("index." <> base.extension))
      idxText

  -- One file per slice, with ToC enabled for navigation.
  --
  -- The sidebar style is HTML-only — pandoc's gfm/commonmark writer
  -- preserves raw <style> blocks verbatim, so emitting the same meta
  -- for every format would leak CSS as visible text in the markdown
  -- output. Gate by format name.
  forM_ ss $ \s -> do
    let
      crumbs =
        breadcrumbs
          [ (B.text "dear-imgui API", Just (rootHref base 1))
          , (B.text "Slices", Just (slicesIndexHref base 1))
          , (B.code s.qualifier, Nothing)
          ]
      withStyle
        | format.name `elem` ["html5", "html"] =
            B.setMeta "header-includes" sliceSidebarStyle
        | otherwise = id
      doc = withStyle (Render.Slice.renderSlice ctx crumbs s)
      path = dir </> Text.unpack (s.qualifier <> "." <> base.extension)
    text <- format.write tocOpts doc
    liftIO $ Text.writeFile path text

{- | Inline @\<style\>@ block injected into slice pages. Floats the
auto-generated ToC (@nav#TOC@) into a sticky right-hand sidebar on
viewports wide enough to fit it; on narrow screens the rules don't
apply, so the ToC reverts to its default block layout above the
content. HTML5-only; other writers see a no-op raw block.
-}
sliceSidebarStyle :: Blocks
sliceSidebarStyle =
  B.rawBlock "html" $
    Text.unlines
      [ "<style>"
      , "@media (min-width: 900px) {"
      , "  body { max-width: 64em; }"
      , "  nav#TOC {"
      , "    float: right;"
      , "    position: sticky;"
      , "    top: 1em;"
      , "    width: 16em;"
      , "    max-height: calc(100vh - 2em);"
      , "    overflow-y: auto;"
      , "    margin: 0 0 1em 1.5em;"
      , "    padding-left: 1em;"
      , "    border-left: 2px solid #e6e6e6;"
      , "    font-size: 0.9em;"
      , "  }"
      , "}"
      , "</style>"
      ]

slicesIndexDoc :: LinkBase -> [Slice] -> Pandoc.Pandoc
slicesIndexDoc base ss =
  B.setMeta "include-before" crumbs $
    B.setMeta "pagetitle" (B.text "Slices") $
      B.doc body
  where
    crumbs =
      breadcrumbs
        [ (B.text "dear-imgui API", Just (rootHref base 1))
        , (B.text "Slices", Nothing)
        ]
    body =
      B.header 1 (B.text "Slices")
        <> case ss of
          [] -> B.para (B.emph (B.text "(none)"))
          _ ->
            let
              byName = Map.fromList [(s.qualifier, s) | s <- ss]
              grouped = groupByQualifier [s.qualifier | s <- ss]
            in
              mconcat (map (renderGroup byName) grouped)

    renderGroup :: Map.Map Text Slice -> (Maybe Text, [(Text, Text)]) -> Blocks
    renderGroup byName (qual, entries) =
      let listing = B.bulletList (map (item byName) entries)
      in case qual of
           Nothing -> listing
           Just q -> B.header 2 (B.code q) <> listing

    item :: Map.Map Text Slice -> (Text, Text) -> Blocks
    item byName (full, short) =
      let countLabel = maybe "" sliceCountLabel (Map.lookup full byName)
      in B.plain $
           B.link (sliceHref base 1 full) "" (B.code short)
             <> B.space
             <> B.text countLabel

sliceCountLabel :: Slice -> Text
sliceCountLabel s =
  let parts =
        [ countOf "struct" (length (maybeToList s.struct))
        , countOf "enum" (length s.enums)
        , countOf "function" (length s.functions)
        , countOf "define" (length s.defines)
        , countOf "typedef" (length s.typedefs)
        ]
  in "(" <> Text.intercalate ", " (filter (not . Text.null) parts) <> ")"
  where
    countOf :: Text -> Int -> Text
    countOf _ 0 = ""
    countOf word 1 = "1 " <> word
    countOf word n = Text.pack (show n) <> " " <> word <> "s"

-- ---------------------------------------------------------------------------
-- Index documents

rootDoc :: LinkBase -> Catalog -> [Slice] -> Pandoc.Pandoc
rootDoc base catalog ss =
  B.setMeta "pagetitle" (B.text "dear-imgui API") $
    B.doc $
      B.header 1 (B.text "dear-imgui API")
        <> B.bulletList (map categoryEntry allCategories ++ [sliceEntry])
  where
    categoryEntry cat =
      B.plain $
        B.link (categoryHref base 0 cat) "" (B.text (categoryLabel cat))
          <> B.space
          <> B.text ("(" <> Text.pack (show (catalogCount cat catalog)) <> ")")
    sliceEntry =
      B.plain $
        B.link (slicesIndexHref base 0) "" (B.text "Slices")
          <> B.space
          <> B.text ("(" <> Text.pack (show (length ss)) <> ")")

categoryDoc :: LinkBase -> Category -> [Text] -> Pandoc.Pandoc
categoryDoc base cat names =
  B.setMeta "include-before" crumbs $
    B.setMeta "pagetitle" (B.text (categoryLabel cat)) $
      B.doc body
  where
    crumbs =
      breadcrumbs
        [ (B.text "dear-imgui API", Just (rootHref base 1))
        , (B.text (categoryLabel cat), Nothing)
        ]
    body =
      B.header 1 (B.text (categoryLabel cat))
        <> case names of
          [] -> B.para (B.emph (B.text "(none)"))
          _ -> mconcat (map renderGroup (groupByQualifier names))
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
