{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

module DearBindings.JSON.Conditional
  ( Conditional (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import DearBindings.JSON.Internal.Options (jsonOptions)
import GHC.Generics (Generic)

data Conditional = Conditional
  { condition :: Text
  , expression :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Conditional where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Conditional where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
