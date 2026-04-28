{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

{-| Internal: shared 'Aeson.Options' for every record in the schema.
Each record uses unprefixed field names; fields named @type_@ are
mapped back to the JSON key @type@ (Haskell keyword workaround).
-}
module DearBindings.JSON.Internal.Options
  ( jsonOptions
  ) where

import Data.Aeson qualified as Aeson

jsonOptions :: Aeson.Options
jsonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \case
        "type_" -> "type"
        s -> Aeson.camelTo2 '_' s
    , Aeson.omitNothingFields = True
    }
