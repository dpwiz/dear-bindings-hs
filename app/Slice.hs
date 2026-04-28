{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| A 'Slice' is the cross-category bundle of everything sharing a
qualifier: the struct (if one exists with that name), the methods
hanging off it, the flag-enums in its orbit, and any defines / typedefs
prefixed with the qualifier. It's the "by namespace / class" lens on
the catalog, complementing the per-category indices in "Generate".
-}
module Slice
  ( Slice (..)
  , slices
  , sliceSize
  ) where

import Catalog (Catalog (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Define (..)
  , Enum_ (..)
  , Function (..)
  , Struct (..)
  , Typedef (..)
  )
import Render.Common (splitQualifier)

data Slice = Slice
  { qualifier :: Text
  , struct :: Maybe Struct
  , enums :: [Enum_]
  , functions :: [Function]
  , defines :: [Define]
  , typedefs :: [Typedef]
  }

{- | Compute every qualifier slice from a 'Catalog', sorted alphabetically.
Slices that contain only a single entity are dropped — they'd just
duplicate the entity's own page in the matching category index, which
is already reachable via @structs\/X.html@ etc.
-}
slices :: Catalog -> [Slice]
slices c =
  [ s
  | q <- Set.toAscList (sliceKeys c)
  , let s = buildSlice c q
  , sliceSize s > 1
  ]

{- | Total number of entities in a slice (struct counts as one if
present, plus the lengths of the enum/function/define/typedef lists).
-}
sliceSize :: Slice -> Int
sliceSize s =
  (if isJust s.struct then 1 else 0)
    + length s.enums
    + length s.functions
    + length s.defines
    + length s.typedefs

{- | The set of keys we render slice pages for. Sources, unioned:

* Every qualifier extracted from a function name (last-underscore
  split).
* Every struct name. (So a struct without methods still gets a slice
  page — useful for plain data types like 'ImVec2'.)
-}
sliceKeys :: Catalog -> Set.Set Text
sliceKeys c =
  Set.fromList (Map.keys c.structs)
    `Set.union` Set.fromList (mapMaybe (fst . splitQualifier) (Map.keys c.functions))

buildSlice :: Catalog -> Text -> Slice
buildSlice c q =
  Slice
    { qualifier = q
    , struct = Map.lookup q c.structs
    , enums = filterByPrefix q c.enums
    , functions =
        [ f
        | (n, f) <- Map.toAscList c.functions
        , fst (splitQualifier n) == Just q
        ]
    , defines = filterDefines q c.defines
    , typedefs = filterByPrefix q c.typedefs
    }

{- | Keep entries whose name starts with the qualifier (covers
@ImDrawListFlags_@ for @ImDrawList@, @ImGuiCol_@ / @ImGuiCond_@ for
@ImGui@, etc.).
-}
filterByPrefix :: Text -> Map Text a -> [a]
filterByPrefix q m =
  [v | (n, v) <- Map.toAscList m, q `Text.isPrefixOf` n]

{- | Defines need a tweak: their names are usually @ALL_CAPS@, while
qualifiers are @MixedCase@. Match by either the qualifier or its
upper-cased form, both followed by an underscore. So the @ImGui@
slice picks up @IMGUI_VERSION@, @IMGUI_HAS_TABLE@, and friends.
-}
filterDefines :: Text -> Map Text Define -> [Define]
filterDefines q m =
  let
    mixed = q <> "_"
    upper = Text.toUpper q <> "_"
  in
    [ d
    | (n, d) <- Map.toAscList m
    , mixed `Text.isPrefixOf` n || upper `Text.isPrefixOf` n
    ]
