
{-| Decide which catalog structs we'll render as Haskell records
with full 'Storable' instances (peek/poke per field), instead of
opaque @data X@. Whiteboxing a struct lets it cross the @capi@ FFI
boundary by value — unlocking functions that would otherwise be
skipped under 'FFI.Skip.ReasonByValueStruct'.

A struct is whiteboxable iff every field has a representable Haskell
type at the FFI boundary: no bitfields, no inline anonymous
struct/union members, no array fields (v0 limitation), and the
struct itself is concrete (not forward-declared, not anonymous).
-}
module FFI.Whitebox
  ( isWhiteboxable
  , whiteboxSet
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import DearBindings.Catalog (Catalog (..))
import DearBindings.JSON
  ( Struct (..)
  , StructField (..)
  , TypeRef (..)
  )
import DearBindings.JSON.Types qualified
import FFI.HType (typeKindContainsInlineAggregate)

-- | All catalog structs we plan to whitebox in this run. v0.1 picks
-- the by-value subset (those that need to cross @capi@ by value)
-- intersected with the tractable-shape filter.
whiteboxSet :: Catalog -> Set Text
whiteboxSet c =
  Set.fromList
    [ s.name
    | s <- Map.elems c.structs
    , s.byValue
    , isWhiteboxable s
    ]

-- | Whitebox-eligibility for a single struct.
isWhiteboxable :: Struct -> Bool
isWhiteboxable s =
  not s.forwardDeclaration
    && not s.isAnonymous
    && not (null s.fields)
    && all isWhiteboxableField s.fields

isWhiteboxableField :: StructField -> Bool
isWhiteboxableField f =
  not (isJust f.width) -- no bitfields
    && not f.isArray -- no array fields (v0)
    && not f.isAnonymous -- no anonymous nested aggregates
    && not (typeKindContainsInlineAggregate f.type_.description)
