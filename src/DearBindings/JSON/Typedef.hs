{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

module DearBindings.JSON.Typedef
  ( Typedef (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import DearBindings.JSON.Comments (Comments)
import DearBindings.JSON.Conditional (Conditional)
import DearBindings.JSON.Internal.Options (jsonOptions)
import DearBindings.JSON.SourceLocation (SourceLocation)
import DearBindings.JSON.Types (TypeRef)
import GHC.Generics (Generic)

data Typedef = Typedef
  { name :: Text
  , type_ :: TypeRef
  , isInternal :: Bool
  , comments :: Maybe Comments
  , conditionals :: Maybe [Conditional]
  , sourceLocation :: SourceLocation
  }
  deriving (Eq, Show, Generic)

instance FromJSON Typedef where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Typedef where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
