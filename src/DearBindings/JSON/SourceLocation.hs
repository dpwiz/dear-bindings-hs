{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

module DearBindings.JSON.SourceLocation
  ( SourceLocation (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import DearBindings.JSON.Internal.Options (jsonOptions)
import GHC.Generics (Generic)

data SourceLocation = SourceLocation
  { filename :: Text
  , line :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON SourceLocation where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON SourceLocation where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
