{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| A 'Slice' is the cross-category bundle of everything sharing a
qualifier: the struct (if one exists with that name), the methods
hanging off it, the flag-enums in its orbit, and any defines / typedefs
prefixed with the qualifier. It's the "by namespace / class" lens on
the catalog, complementing the per-category indices used by the doc
generator.

Two entry points:

* 'slices' — drops single-entity slices. The doc generator uses this
  because a one-element slice page just duplicates the entity's
  category-index entry.
* 'slicesAll' — keeps every slice, including singletons. The FFI
  generator uses this because every C symbol must end up bound in
  some module.

The membership predicates ('structInSlice' & friends) are exported so
downstream tools can compute "which slice does this name belong to?"
without re-deriving the rule.
-}
module DearBindings.Slice
  ( Slice (..)
  , slices
  , slicesAll
  , sliceSize

    -- * Membership predicates
  , structInSlice
  , functionInSlice
  , enumInSlice
  , typedefInSlice
  , defineInSlice
  ) where

import DearBindings.Catalog (Catalog (..))
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
import DearBindings.Qualifier (splitQualifier)

data Slice = Slice
  { qualifier :: Text
  , struct :: Maybe Struct
  , enums :: [Enum_]
  , functions :: [Function]
  , defines :: [Define]
  , typedefs :: [Typedef]
  }

-- ---------------------------------------------------------------------------
-- Membership predicates: do entities of a given category, named @n@,
-- belong to the slice for qualifier @q@? Single-sourced here so that
-- 'buildSlice' (page contents) and downstream symbol-table builders
-- agree on the rule.

structInSlice :: Text -> Text -> Bool
structInSlice q n = q == n

functionInSlice :: Text -> Text -> Bool
functionInSlice q n = fst (splitQualifier n) == Just q

enumInSlice :: Text -> Text -> Bool
enumInSlice q n = q `Text.isPrefixOf` n

typedefInSlice :: Text -> Text -> Bool
typedefInSlice q n = q `Text.isPrefixOf` n

{- | Defines need a tweak: their names are usually @ALL_CAPS@, while
qualifiers are @MixedCase@. Match by either the qualifier or its
upper-cased form, both followed by an underscore. So the @ImGui@
slice picks up @IMGUI_VERSION@, @IMGUI_HAS_TABLE@, and friends.
-}
defineInSlice :: Text -> Text -> Bool
defineInSlice q n =
  (q <> "_") `Text.isPrefixOf` n
    || (Text.toUpper q <> "_") `Text.isPrefixOf` n

{- | Compute every qualifier slice from a 'Catalog', sorted alphabetically.
Slices that contain only a single entity are dropped — they'd just
duplicate the entity's own page in the matching category index, which
is already reachable via @structs\/X.html@ etc. Use 'slicesAll' if
you want every slice including singletons.
-}
slices :: Catalog -> [Slice]
slices c =
  [ s
  | q <- Set.toAscList (sliceKeys c)
  , let s = buildSlice c q
  , sliceSize s > 1
  ]

{- | Like 'slices' but keeps single-entity slices. Use this when every
qualifier needs to map to some output, even if there's only one
entity in it (e.g. FFI binding generation, where every C symbol
must be bound somewhere).
-}
slicesAll :: Catalog -> [Slice]
slicesAll c =
  [ buildSlice c q
  | q <- Set.toAscList (sliceKeys c)
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
    , enums = filterMap (enumInSlice q) c.enums
    , functions = filterMap (functionInSlice q) c.functions
    , defines = filterMap (defineInSlice q) c.defines
    , typedefs = filterMap (typedefInSlice q) c.typedefs
    }

filterMap :: (Text -> Bool) -> Map Text a -> [a]
filterMap p m = [v | (n, v) <- Map.toAscList m, p n]
