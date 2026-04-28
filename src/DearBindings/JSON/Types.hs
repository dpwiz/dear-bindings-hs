{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The mutually-recursive cluster of the schema. These six types form a
cycle ('TypeRef' ↔ 'TypeKind' ↔ 'Struct' ↔ 'StructField' ↔ 'TypeRef',
and 'TypeRef' ↔ 'TypeDetails' ↔ 'Argument' ↔ 'TypeRef'), so they have
to share a module — Haskell does not let mutually-recursive data types
live in separate modules without @.hs-boot@ files.
-}
module DearBindings.JSON.Types
  ( TypeRef (..)
  , TypeDetails (..)
  , TypeKind (..)
  , Argument (..)
  , Struct (..)
  , StructField (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import DearBindings.JSON.Comments (Comments)
import DearBindings.JSON.Conditional (Conditional)
import DearBindings.JSON.Internal.Options (jsonOptions)
import DearBindings.JSON.SourceLocation (SourceLocation)
import GHC.Generics (Generic)

data TypeRef = TypeRef
  { declaration :: Text
  , description :: TypeKind
  , typeDetails :: Maybe TypeDetails
  }
  deriving (Eq, Show, Generic)

instance FromJSON TypeRef where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON TypeRef where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions

{- | Auxiliary view of a type that the parser provides for opaque cases —
currently only function pointers (@flavour = "function_pointer"@) get
one. The @description@ tree already encodes the same information, but
@type_details@ flattens it for consumers that don't want to traverse
the recursive 'TypeKind'.
-}
data TypeDetails = TypeDetails
  { flavour :: Text
  , returnType :: TypeRef
  , arguments :: [Argument]
  }
  deriving (Eq, Show, Generic)

instance FromJSON TypeDetails where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON TypeDetails where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions

data Argument = Argument
  { name :: Maybe Text
  , type_ :: Maybe TypeRef
  , isArray :: Bool
  , isVarargs :: Bool
  , isInstancePointer :: Bool
  , defaultValue :: Maybe Text
  , arrayBounds :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Argument where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Argument where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions

data Struct = Struct
  { name :: Text
  , originalFullyQualifiedName :: Text
  , kind :: Text
  , byValue :: Bool
  , forwardDeclaration :: Bool
  , isAnonymous :: Bool
  , isInternal :: Bool
  , fields :: [StructField]
  , comments :: Maybe Comments
  , conditionals :: Maybe [Conditional]
  , sourceLocation :: SourceLocation
  }
  deriving (Eq, Show, Generic)

instance FromJSON Struct where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON Struct where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions

data StructField = StructField
  { name :: Text
  , type_ :: TypeRef
  , isArray :: Bool
  , isAnonymous :: Bool
  , isInternal :: Bool
  , arrayBounds :: Maybe Text
  , defaultValue :: Maybe Text
  , width :: Maybe Int
  , comments :: Maybe Comments
  , conditionals :: Maybe [Conditional]
  , sourceLocation :: SourceLocation
  }
  deriving (Eq, Show, Generic)

instance FromJSON StructField where parseJSON = Aeson.genericParseJSON jsonOptions
instance ToJSON StructField where
  toJSON = Aeson.genericToJSON jsonOptions
  toEncoding = Aeson.genericToEncoding jsonOptions

{- | A type description tree node. The JSON discriminator @kind@ uses PascalCase
for non-aggregate kinds and lowercase for inline @struct@/@union@; for the
two aggregate cases the constructor's payload is the full 'Struct' shape
flattened next to @kind@. That mismatch makes a Generic-derived sum
encoding awkward, so the instances are written by hand.
-}
data TypeKind
  = TKBuiltin Text (Maybe [Text])
  | -- | inner_type, is_nullable, is_reference, storage_classes
    TKPointer TypeKind (Maybe Bool) (Maybe Bool) (Maybe [Text])
  | -- | inner_type, bounds
    TKArray TypeKind (Maybe Text)
  | -- | name, storage_classes
    TKUser Text (Maybe [Text])
  | -- | name, inner_type
    TKType Text TypeKind
  | {- | return_type (a bare description tree here, not a 'TypeRef'),
    parameters (each is a 'TKType' wrapping the parameter's inner type)
    -}
    TKFunction TypeKind [TypeKind]
  | TKInlineStruct Struct
  | TKInlineUnion Struct
  deriving (Eq, Show, Generic)

instance FromJSON TypeKind where
  parseJSON = Aeson.withObject "TypeKind" $ \o -> do
    k :: Text <- o .: "kind"
    case k of
      "Builtin" ->
        TKBuiltin
          <$> o .: "builtin_type"
          <*> o .:? "storage_classes"
      "Pointer" ->
        TKPointer
          <$> o .: "inner_type"
          <*> o .:? "is_nullable"
          <*> o .:? "is_reference"
          <*> o .:? "storage_classes"
      "Array" ->
        TKArray
          <$> o .: "inner_type"
          <*> o .:? "bounds"
      "User" ->
        TKUser
          <$> o .: "name"
          <*> o .:? "storage_classes"
      "Type" ->
        TKType
          <$> o .: "name"
          <*> o .: "inner_type"
      "Function" ->
        TKFunction
          <$> o .: "return_type"
          <*> o .: "parameters"
      "struct" -> TKInlineStruct <$> parseJSON (Aeson.Object o)
      "union" -> TKInlineUnion <$> parseJSON (Aeson.Object o)
      other -> fail ("Unknown TypeKind kind: " <> show other)

instance ToJSON TypeKind where
  toJSON = \case
    TKBuiltin t scs ->
      Aeson.object $
        [ "kind" .= ("Builtin" :: Text)
        , "builtin_type" .= t
        ]
          <> optField "storage_classes" scs
    TKPointer inner nullable ref scs ->
      Aeson.object $
        [ "kind" .= ("Pointer" :: Text)
        , "inner_type" .= inner
        ]
          <> optField "is_nullable" nullable
          <> optField "is_reference" ref
          <> optField "storage_classes" scs
    TKArray inner bounds ->
      Aeson.object $
        [ "kind" .= ("Array" :: Text)
        , "inner_type" .= inner
        ]
          <> optField "bounds" bounds
    TKUser n scs ->
      Aeson.object $
        [ "kind" .= ("User" :: Text)
        , "name" .= n
        ]
          <> optField "storage_classes" scs
    TKType n inner ->
      Aeson.object
        [ "kind" .= ("Type" :: Text)
        , "name" .= n
        , "inner_type" .= inner
        ]
    TKFunction rt ps ->
      Aeson.object
        [ "kind" .= ("Function" :: Text)
        , "return_type" .= rt
        , "parameters" .= ps
        ]
    TKInlineStruct s -> reKind "struct" (toJSON s)
    TKInlineUnion s -> reKind "union" (toJSON s)

optField :: (ToJSON a) => Key.Key -> Maybe a -> [Pair]
optField _ Nothing = []
optField k (Just v) = [k .= v]

-- | Replace the @kind@ field in an already-encoded 'Struct' object.
reKind :: Text -> Aeson.Value -> Aeson.Value
reKind k = \case
  Aeson.Object o -> Aeson.Object (KM.insert "kind" (Aeson.String k) o)
  v -> v
