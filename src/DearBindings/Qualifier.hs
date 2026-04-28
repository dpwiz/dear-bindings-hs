{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Names and qualifiers shared by every consumer of a parsed dear-bindings
header. Two pieces:

* 'Category' — the five top-level kinds an entity can have, mirroring
  the five list fields of 'DearBindings.JSON.Header' one-to-one.
* 'splitQualifier' — the canonical "split a name on its last underscore"
  rule used to recover the C++ class qualifier from a flattened C name.

Lives in the library so that downstream tools (the doc generator, the
FFI generator, anything else) can share the rule without depending on
each other.
-}
module DearBindings.Qualifier
  ( Category (..)
  , splitQualifier
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

{- | The five top-level categories an entity can live under. Mirrors the
five fields of 'DearBindings.JSON.Header' one-to-one.
-}
data Category = Defines | Enums | Typedefs | Structs | Functions
  deriving (Eq, Ord, Show, Bounded, Enum)

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
