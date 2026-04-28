{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Note: the type is named 'Enum_' (trailing underscore) to avoid
clashing with @Prelude.Enum@.
-}
module DearBindings.JSON.Enum
  ( Enum_ (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import DearBindings.JSON.Comments (Comments)
import DearBindings.JSON.Conditional (Conditional)
import DearBindings.JSON.EnumElement (EnumElement)
import DearBindings.JSON.Internal.Options (jsonOptions)
import DearBindings.JSON.SourceLocation (SourceLocation)
import DearBindings.JSON.Types (TypeRef)
import GHC.Generics (Generic)

data Enum_ = Enum_
  { name :: Text
  , originalFullyQualifiedName :: Text
  , storageType :: Maybe TypeRef
  , isFlagsEnum :: Bool
  , isInternal :: Bool
  , elements :: [EnumElement]
  , comments :: Maybe Comments
  , conditionals :: Maybe [Conditional]
  , sourceLocation :: SourceLocation
  }
  deriving (Eq, Show, Generic)

instance FromJSON Enum_ where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Enum_ where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
