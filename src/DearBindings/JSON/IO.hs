{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

{-| Monomorphic Aeson wrappers for 'Header' — handy in tests and
one-shot scripts that don't want type applications everywhere.
-}
module DearBindings.JSON.IO
  ( -- * Decode
    eitherDecode
  , eitherDecodeFile
  , decodeFile

    -- * Encode
  , encode
  , encodeFile
  ) where

import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import DearBindings.JSON.Header (Header)

-- | Decode a 'Header' from a lazy 'ByteString'.
eitherDecode :: ByteString -> Either String Header
eitherDecode = Aeson.eitherDecode

-- | Decode a 'Header' from a JSON file.
eitherDecodeFile :: FilePath -> IO (Either String Header)
eitherDecodeFile = Aeson.eitherDecodeFileStrict'

-- | Decode a 'Header' from a JSON file; throws 'userError' on failure.
decodeFile :: FilePath -> IO Header
decodeFile fp =
  eitherDecodeFile fp >>= \case
    Right h -> pure h
    Left e -> throwIO (userError ("decodeFile " <> fp <> ": " <> e))

-- | Encode a 'Header' to a lazy 'ByteString'.
encode :: Header -> ByteString
encode = Aeson.encode

-- | Encode a 'Header' and write it to a file.
encodeFile :: FilePath -> Header -> IO ()
encodeFile fp = LBS.writeFile fp . encode
