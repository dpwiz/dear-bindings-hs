{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

module DearBindings.JSON.Comments
  ( Comments (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import DearBindings.JSON.Internal.Options (jsonOptions)
import GHC.Generics (Generic)

data Comments = Comments
  { preceding :: Maybe [Text]
  , attached :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Comments where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Comments where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
