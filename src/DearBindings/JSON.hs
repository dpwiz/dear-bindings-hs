{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Umbrella re-export of every type in the dear_bindings JSON schema.
Import 'DearBindings.JSON.IO' separately for monomorphic decode/encode
helpers.
-}
module DearBindings.JSON
  ( -- * Top level
    Header (..)

    -- * Top-level entries
  , Define (..)
  , Enum_ (..)
  , EnumElement (..)
  , Typedef (..)
  , Function (..)

    -- * Type-tree cluster
  , Struct (..)
  , StructField (..)
  , Argument (..)
  , TypeRef (..)
  , TypeDetails (..)
  , TypeKind (..)

    -- * Leaves
  , Comments (..)
  , Conditional (..)
  , SourceLocation (..)
  ) where

import DearBindings.JSON.Comments (Comments (..))
import DearBindings.JSON.Conditional (Conditional (..))
import DearBindings.JSON.Define (Define (..))
import DearBindings.JSON.Enum (Enum_ (..))
import DearBindings.JSON.EnumElement (EnumElement (..))
import DearBindings.JSON.Function (Function (..))
import DearBindings.JSON.Header (Header (..))
import DearBindings.JSON.SourceLocation (SourceLocation (..))
import DearBindings.JSON.Typedef (Typedef (..))
import DearBindings.JSON.Types
  ( Argument (..)
  , Struct (..)
  , StructField (..)
  , TypeDetails (..)
  , TypeKind (..)
  , TypeRef (..)
  )
