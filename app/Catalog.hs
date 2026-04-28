{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Merged in-memory view of one-or-more 'Header' inputs. The catalog
is keyed by entity name within each category; dear-bindings makes
those names globally unique within a release, so a flat merge is
safe.
-}
module Catalog
  ( Catalog (..)
  , empty
  , fromHeader
  , fromHeaders

    -- * Filtering
  , Filter (..)
  , defaultFilter
  , applyFilter
  , size
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DearBindings.JSON
  ( Define (..)
  , Enum_ (..)
  , Function (..)
  , Header (..)
  , Struct (..)
  , Typedef (..)
  )
import Render.Common (Category (..))

data Catalog = Catalog
  { defines :: Map Text Define
  , enums :: Map Text Enum_
  , typedefs :: Map Text Typedef
  , structs :: Map Text Struct
  , functions :: Map Text Function
  }
  deriving (Eq, Show)

empty :: Catalog
empty = Catalog Map.empty Map.empty Map.empty Map.empty Map.empty

fromHeader :: Header -> Catalog
fromHeader h =
  Catalog
    { defines = byName (.name) h.defines
    , enums = byName (.name) h.enums
    , typedefs = byName (.name) h.typedefs
    , structs = byName (.name) h.structs
    , functions = byName (.name) h.functions
    }
  where
    byName key = Map.fromList . map (\x -> (key x, x))

fromHeaders :: [Header] -> Catalog
fromHeaders = foldr (\h acc -> merge (fromHeader h) acc) empty
  where
    merge :: Catalog -> Catalog -> Catalog
    merge a b =
      Catalog
        { defines = a.defines `Map.union` b.defines
        , enums = a.enums `Map.union` b.enums
        , typedefs = a.typedefs `Map.union` b.typedefs
        , structs = a.structs `Map.union` b.structs
        , functions = a.functions `Map.union` b.functions
        }

{- | What 'applyFilter' selects. An empty 'categories' set means "all
categories"; empty 'names' and 'patterns' mean "all entries within
the chosen categories". Otherwise, an entry passes if it's in any
of the named categories AND its name matches any of the names or
substring patterns.
-}
data Filter = Filter
  { categories :: Set Category
  , names :: [Text] -- exact match
  , patterns :: [Text] -- case-insensitive substring
  }
  deriving (Eq, Show)

defaultFilter :: Filter
defaultFilter = Filter Set.empty [] []

applyFilter :: Filter -> Catalog -> Catalog
applyFilter f c =
  Catalog
    { defines = ifIn Defines c.defines
    , enums = ifIn Enums c.enums
    , typedefs = ifIn Typedefs c.typedefs
    , structs = ifIn Structs c.structs
    , functions = ifIn Functions c.functions
    }
  where
    ifIn cat m
      | not (Set.null f.categories) && cat `Set.notMember` f.categories = Map.empty
      | otherwise = Map.filterWithKey (\k _ -> matchesName f k) m

matchesName :: Filter -> Text -> Bool
matchesName f name
  | null f.names && null f.patterns = True
  | otherwise = exact || pattern_
  where
    exact = name `elem` f.names
    pattern_ = any (\p -> Text.toLower p `Text.isInfixOf` Text.toLower name) f.patterns

-- | Total entry count, useful for tests / status messages.
size :: Catalog -> Int
size c =
  Map.size c.defines
    + Map.size c.enums
    + Map.size c.typedefs
    + Map.size c.structs
    + Map.size c.functions
