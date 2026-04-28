{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

module DearBindings.JSON.EnumElement
  ( EnumElement (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import DearBindings.JSON.Comments (Comments)
import DearBindings.JSON.Conditional (Conditional)
import DearBindings.JSON.Internal.Options (jsonOptions)
import DearBindings.JSON.SourceLocation (SourceLocation)
import GHC.Generics (Generic)

data EnumElement = EnumElement
  { name :: Text
  , value :: Int
  , valueExpression :: Maybe Text
  , isCount :: Bool
  , isInternal :: Bool
  , comments :: Maybe Comments
  , conditionals :: Maybe [Conditional]
  , sourceLocation :: SourceLocation
  }
  deriving (Eq, Show, Generic)

instance FromJSON EnumElement where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON EnumElement where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
