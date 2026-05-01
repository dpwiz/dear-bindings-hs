module App.FileBrowser
  ( BrowserState (..)
  , Entry (..)
  , Preview (..)
  , newBrowser
  , listEntries
  , loadPreview
  , goInto
  , goUp
  , textPreviewLimit
  , binaryPreviewLimit
  ) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import System.Directory qualified as Dir
import System.FilePath ((</>), isDrive, takeDirectory)

data BrowserState = BrowserState
  { cwd      :: !(IORef FilePath)
  , entries  :: !(IORef [Entry])
  , selected :: !(IORef (Maybe FilePath))
  , preview  :: !(IORef Preview)
  }

data Entry = Entry
  { name  :: !FilePath
  , isDir :: !Bool
  }
  deriving (Eq, Show)

data Preview
  = NoPreview
  | TextPreview !FilePath !Text
  | BinaryPreview !FilePath !Int !ByteString
    -- ^ full size + first N bytes
  | ErrorPreview !FilePath !Text
  deriving (Eq, Show)

textPreviewLimit :: Int
textPreviewLimit = 256 * 1024

binaryPreviewLimit :: Int
binaryPreviewLimit = 256

readCap :: Int
readCap = 1024 * 1024

newBrowser :: FilePath -> IO BrowserState
newBrowser start = do
  start' <- Dir.canonicalizePath start
  cwdRef     <- newIORef start'
  es         <- listEntries start'
  entriesRef <- newIORef es
  selRef     <- newIORef Nothing
  prevRef    <- newIORef NoPreview
  pure BrowserState
    { cwd      = cwdRef
    , entries  = entriesRef
    , selected = selRef
    , preview  = prevRef
    }

listEntries :: FilePath -> IO [Entry]
listEntries dir = do
  result <- try @IOException (Dir.listDirectory dir)
  case result of
    Left _ -> pure []
    Right names -> do
      annotated <- mapM annotate names
      pure $ sortOn sortKey annotated
  where
    annotate n = do
      d <- Dir.doesDirectoryExist (dir </> n)
      pure Entry { name = n, isDir = d }
    sortKey e = (not e.isDir, map toLowerCh e.name)
    toLowerCh c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c

goInto :: FilePath -> FilePath -> IO FilePath
goInto base sub = Dir.canonicalizePath (base </> sub)

goUp :: FilePath -> FilePath
goUp p
  | isDrive p = p
  | otherwise = takeDirectory p

loadPreview :: FilePath -> IO Preview
loadPreview path = do
  result <- try @IOException $ do
    sz <- Dir.getFileSize path
    h  <- BS.readFile path
    pure (sz, h)
  case result of
    Left e -> pure $ ErrorPreview path (Text.pack (show e))
    Right (sz, bytes) ->
      let truncated = BS.take readCap bytes
       in pure $ classify path (fromIntegral sz) truncated

classify :: FilePath -> Int -> ByteString -> Preview
classify path size bs
  | BS.null bs = TextPreview path Text.empty
  | BS.elem 0 bs = BinaryPreview path size (BS.take binaryPreviewLimit bs)
  | otherwise =
      case Text.decodeUtf8' bs of
        Right t  -> TextPreview path (Text.take textPreviewLimit t)
        Left _   -> case Text.decodeUtf8With Text.lenientDecode bs of
          t | Text.any (== '\xFFFD') (Text.take 64 t)
              -> BinaryPreview path size (BS.take binaryPreviewLimit bs)
            | otherwise
              -> TextPreview path (Text.take textPreviewLimit t)
