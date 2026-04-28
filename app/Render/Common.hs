{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Shared rendering helpers used by every per-record renderer in
"Render". Two flavours of helper:

  * Display: 'commentBlocks', 'sourceFooter', 'anchor', 'attr',
    'linkifyDecl'.
  * Linking: 'LinkBase', 'href', 'entityHref', 'categoryHref',
    'rootHref', 'sliceHref', 'symbolHref' — used both by index-file
    builders in "Generate" and by the renderers themselves to turn
    type names in signatures into clickable links.
-}
module Render.Common
  ( -- * Categories
    Category (..)
  , categoryDir
  , categoryLabel
  , categoryAnchorPrefix

    -- * Linking
  , LinkBase (..)
  , href
  , entityHref
  , categoryHref
  , rootHref
  , slicesDir
  , sliceHref
  , slicesIndexHref

    -- * Symbol table / link context
  , SymbolTable
  , LinkContext (..)
  , emptyContext
  , symbolHref
  , linkifyDecl

    -- * Names / qualifiers
  , splitQualifier
  , groupByQualifier

    -- * Body helpers
  , commentBlocks
  , sourceFooter
  , anchor
  , attr
  , breadcrumbs
  ) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (intersperse, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON (Comments (..), SourceLocation (..))
import Text.Pandoc.Builder (Blocks, Inlines)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Attr)

{- | The five top-level categories an entity can live under. Mirrors the
five fields of 'DearBindings.JSON.Header' one-to-one.
-}
data Category = Defines | Enums | Typedefs | Structs | Functions
  deriving (Eq, Ord, Show, Bounded, Enum)

categoryDir :: Category -> Text
categoryDir = \case
  Defines -> "defines"
  Enums -> "enums"
  Typedefs -> "typedefs"
  Structs -> "structs"
  Functions -> "functions"

categoryLabel :: Category -> Text
categoryLabel = \case
  Defines -> "Defines"
  Enums -> "Enums"
  Typedefs -> "Typedefs"
  Structs -> "Structs"
  Functions -> "Functions"

categoryAnchorPrefix :: Category -> Text
categoryAnchorPrefix = \case
  Defines -> "define"
  Enums -> "enum"
  Typedefs -> "typedef"
  Structs -> "struct"
  Functions -> "function"

{- | How internal links should be formed.

* @basePath = Nothing@ → relative URLs that depend on the page's
  position in the tree (the @depth@ argument to 'href'). This is the
  default; it lets the user open @index.html@ via @file://@ without a
  webserver.

* @basePath = Just "/imgui-docs/"@ → absolute URLs anchored at that
  prefix, regardless of page depth. Use when hosting the output under
  a sub-path on GitHub Pages / GitLab Pages / similar.

The 'extension' is the file extension (without dot) for the chosen
output format — @html@, @md@, @1@, @txt@, @hs@, @native@.
-}
data LinkBase = LinkBase
  { basePath :: Maybe Text
  , extension :: Text
  }
  deriving (Eq, Show)

{- | Build a URL to @target@ (a path within the catalog tree, like
@"functions/ImGui_Begin.html"@) from a page at the given depth
(0 = top index, 1 = category index, 2 = entity page).
-}
href :: LinkBase -> Int -> Text -> Text
href base depth target =
  case base.basePath of
    Just bp -> bp <> target
    Nothing -> Text.replicate depth "../" <> target

-- | URL of a single entity page, relative to a page at @depth@.
entityHref :: LinkBase -> Int -> Category -> Text -> Text
entityHref base depth cat name =
  href base depth (categoryDir cat <> "/" <> name <> "." <> base.extension)

-- | URL of a category's @index@ page, relative to a page at @depth@.
categoryHref :: LinkBase -> Int -> Category -> Text
categoryHref base depth cat =
  href base depth (categoryDir cat <> "/index." <> base.extension)

-- | URL of the top-level @index@ page, relative to a page at @depth@.
rootHref :: LinkBase -> Int -> Text
rootHref base depth = href base depth ("index." <> base.extension)

-- | Subdirectory holding the qualifier-slice pages.
slicesDir :: Text
slicesDir = "slices"

-- | URL of a single slice page, relative to a page at @depth@.
sliceHref :: LinkBase -> Int -> Text -> Text
sliceHref base depth qualifier =
  href base depth (slicesDir <> "/" <> qualifier <> "." <> base.extension)

-- | URL of the slices @index@ page, relative to a page at @depth@.
slicesIndexHref :: LinkBase -> Int -> Text
slicesIndexHref base depth =
  href base depth (slicesDir <> "/index." <> base.extension)

{- | Catalog name → its category and the qualifier of the most-specific
kept slice that contains it (if any). Built once per generate run;
consulted by every renderer that wants to turn an identifier into a
link. 'Nothing' for the slice means "this entity has no kept slice;
link to its dedicated category page".
-}
type SymbolTable = Map Text (Category, Maybe Text)

{- | Everything a renderer needs to emit cross-links: the symbol table,
the URL-shaping policy, and the depth of the page being rendered
(used to compute relative @../@ prefixes when 'LinkBase.basePath' is
'Nothing'). Per-entity pages and slice pages both live one directory
deep, so @depth@ is uniformly @1@ in 'Generate'.
-}
data LinkContext = LinkContext
  { symbols :: SymbolTable
  , base :: LinkBase
  , depth :: Int
  }

{- | Context with no symbols; produced by 'linkifyDecl' it yields the
same plain code spans the old code emitted. Useful in 'Query' (stdout
mode) where links are meaningless.
-}
emptyContext :: LinkContext
emptyContext =
  LinkContext
    { symbols = Map.empty
    , base = LinkBase{basePath = Nothing, extension = ""}
    , depth = 0
    }

{- | URL for a name in the symbol table. Slice match → slice anchor;
otherwise → the entity's dedicated page. 'Nothing' means the name
isn't a known catalog symbol (so the caller leaves it as plain code).
-}
symbolHref :: LinkContext -> Text -> Maybe Text
symbolHref ctx name = do
  (cat, mq) <- Map.lookup name ctx.symbols
  pure $ case mq of
    Just q ->
      sliceHref ctx.base ctx.depth q
        <> "#"
        <> categoryAnchorPrefix cat
        <> "-"
        <> name
    Nothing -> entityHref ctx.base ctx.depth cat name

{- | Render a C declaration string as inline pandoc with identifier
substrings turned into hyperlinks when they appear in the symbol
table. Non-identifier runs (whitespace, @*@, @[@, @,@, …) and
unknown identifiers stay as plain @\<code\>@ spans, so the visual
result is "the same code text, but type names are now blue and
clickable".

Identifier alphabet matches C: @[A-Za-z_][A-Za-z0-9_]*@.
-}
linkifyDecl :: LinkContext -> Text -> Inlines
linkifyDecl ctx = mconcat . map renderChunk . tokenize
  where
    renderChunk :: Either Text Text -> Inlines
    renderChunk (Left lit) = B.code lit
    renderChunk (Right ident) =
      case symbolHref ctx ident of
        Just url -> B.link url "" (B.code ident)
        Nothing -> B.code ident

    tokenize :: Text -> [Either Text Text]
    tokenize t
      | Text.null t = []
      | isIdentStart (Text.head t) =
          let (ident, rest) = Text.span isIdentChar t
          in Right ident : tokenize rest
      | otherwise =
          let (lit, rest) = Text.break isIdentStart t
          in Left lit : tokenize rest

    isIdentStart :: Char -> Bool
    isIdentStart c = isAlpha c || c == '_'

    isIdentChar :: Char -> Bool
    isIdentChar c = isAlphaNum c || c == '_'

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

{- | Group a list of names by their qualifier. The 'Nothing' bucket
(unqualified names) sorts first; remaining buckets are alphabetical.
Each value list is @(full_name, short_name)@ sorted by short name.
-}
groupByQualifier :: [Text] -> [(Maybe Text, [(Text, Text)])]
groupByQualifier names =
  let
    pairs = [(q, (full, short)) | full <- names, let (q, short) = splitQualifier full]
    grouped = Map.fromListWith (++) [(q, [v]) | (q, v) <- pairs]
  in
    [(q, sortOn snd entries) | (q, entries) <- Map.toAscList grouped]

{- | Render the @comments@ field. Preceding lines become a code block
that preserves them verbatim (the source is already prefixed with
@\/\/@; turning each line into a paragraph collapses the line breaks
to spaces and reads as a single run-on sentence). The attached
one-liner becomes an emphasised paragraph.
-}
commentBlocks :: Maybe Comments -> Blocks
commentBlocks Nothing = mempty
commentBlocks (Just c) =
  mconcat $
    catMaybes
      [ codeBlockFrom <$> c.preceding
      , (B.para . B.emph . B.text) <$> c.attached
      ]
  where
    codeBlockFrom :: [Text] -> Blocks
    codeBlockFrom = B.codeBlock . Text.intercalate "\n"

-- | One-line source footer, e.g. @source: imgui.h:1234@.
sourceFooter :: SourceLocation -> Blocks
sourceFooter sl =
  B.para . B.emph . B.text $
    "source: " <> sl.filename <> maybe "" (\n -> ":" <> Text.pack (show n)) sl.line

-- | Build the @id@-only 'Attr' triple for header anchors.
anchor :: Category -> Text -> Attr
anchor cat name =
  (categoryAnchorPrefix cat <> "-" <> name, [], [])

-- | Synonym for an empty 'Attr' (no id, no class, no key/value pairs).
attr :: Attr
attr = ("", [], [])

{- | Render a breadcrumb trail as a single paragraph. Each entry is a
label and an optional URL; entries with @Just@ become hyperlinks,
@Nothing@ stays as plain inlines (use this for the current page).
Entries are joined by @›@. An empty list produces no output.
-}
breadcrumbs :: [(Inlines, Maybe Text)] -> Blocks
breadcrumbs [] = mempty
breadcrumbs items =
  B.para . mconcat . intersperse (B.text " › ") $ map render items
  where
    render :: (Inlines, Maybe Text) -> Inlines
    render (label, Nothing) = label
    render (label, Just url) = B.link url "" label
