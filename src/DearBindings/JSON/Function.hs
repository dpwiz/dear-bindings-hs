{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

module DearBindings.JSON.Function
  ( Function (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import DearBindings.JSON.Comments (Comments)
import DearBindings.JSON.Conditional (Conditional)
import DearBindings.JSON.Internal.Options (jsonOptions)
import DearBindings.JSON.SourceLocation (SourceLocation)
import DearBindings.JSON.Types (Argument, TypeRef)
import GHC.Generics (Generic)

data Function = Function
  { name :: Text
  , originalFullyQualifiedName :: Text
  , originalClass :: Maybe Text
  , returnType :: TypeRef
  , arguments :: [Argument]
  , isInternal :: Bool
  , isStatic :: Bool
  , isImstrHelper :: Bool
  , isManualHelper :: Bool
  , isUnformattedHelper :: Bool
  , isDefaultArgumentHelper :: Bool
  , hasImstrHelper :: Bool
  , comments :: Maybe Comments
  , conditionals :: Maybe [Conditional]
  , sourceLocation :: SourceLocation
  }
  deriving (Eq, Show, Generic)

instance FromJSON Function where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Function where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
