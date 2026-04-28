{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The top-level value of a dear_bindings JSON file. dear_bindings is
run once per C/C++ header, producing one 'Header' per file.
-}
module DearBindings.JSON.Header
  ( Header (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import DearBindings.JSON.Define (Define)
import DearBindings.JSON.Enum (Enum_)
import DearBindings.JSON.Function (Function)
import DearBindings.JSON.Internal.Options (jsonOptions)
import DearBindings.JSON.Typedef (Typedef)
import DearBindings.JSON.Types (Struct)
import GHC.Generics (Generic)

data Header = Header
  { defines :: [Define]
  , enums :: [Enum_]
  , typedefs :: [Typedef]
  , structs :: [Struct]
  , functions :: [Function]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Header where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Header where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions
