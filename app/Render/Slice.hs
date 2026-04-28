{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Render a 'Slice' (the qualifier-keyed cross-category bundle) into a
single pandoc document. Each contained entity is rendered with the
existing per-record renderers from "Render"; their h1 banners are
demoted by two so the page hierarchy is

@
    h1  <qualifier>
    h2  Struct       /  Enums  /  Functions  /  Defines  /  Typedefs
    h3  <entity>
@

— a clean shape for the auto-generated table of contents.
-}
module Render.Slice
  ( renderSlice
  ) where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Render qualified
import Render.Common (LinkContext)
import Slice (Slice (..))
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Walk qualified as Walk

renderSlice :: LinkContext -> Blocks -> Slice -> Pandoc.Pandoc
renderSlice ctx crumbs s =
  B.setMeta "include-before" crumbs $
    B.setMeta "pagetitle" (B.text s.qualifier) $
      B.doc body
  where
    body =
      B.header 1 (B.code s.qualifier)
        <> section "Struct" (Render.renderStruct ctx <$> maybeToList s.struct)
        <> section "Enums" (map (Render.renderEnum ctx) s.enums)
        <> section "Functions" (map (Render.renderFunction ctx) s.functions)
        <> section "Defines" (map (Render.renderDefine ctx) s.defines)
        <> section "Typedefs" (map (Render.renderTypedef ctx) s.typedefs)

section :: Text -> [Blocks] -> Blocks
section _ [] = mempty
section name parts =
  B.header 2 (B.text name) <> demoteHeaders 2 (mconcat parts)

{- | Add @n@ to every header level in a 'Blocks' tree, clamped at 6 (the
HTML5 maximum). Lets us reuse the existing per-record renderers — which
emit h1 — inside an h2 section without producing parallel h1s.
-}
demoteHeaders :: Int -> Blocks -> Blocks
demoteHeaders n =
  Walk.walk $ \case
    Pandoc.Header lvl attr inlines -> Pandoc.Header (min 6 (lvl + n)) attr inlines
    b -> b
