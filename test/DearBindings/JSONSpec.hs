{-# LANGUAGE ImportQualifiedPost #-}

module DearBindings.JSONSpec (tests) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import DearBindings.JSON.IO qualified as JSON
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

variants :: [(String, FilePath)]
variants =
  [ ("vanilla", "dear_bindings/vanilla")
  , ("docking", "dear_bindings/docking")
  ]

tests :: IO TestTree
tests = do
  variantGroups <- mapM variantGroup variants
  pure $ testGroup "DearBindings.JSON round-trip" variantGroups

variantGroup :: (String, FilePath) -> IO TestTree
variantGroup (label, dir) = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure $ testGroup label
      [ testCase "skipped" $ assertFailure $
          "directory missing: " <> dir
          <> " (run scripts/pull_dear_bindings.py to populate)"
      ]
    else do
      files <- filter ((== ".json") . takeExtension) <$> listDirectory dir
      pure $ testGroup label (map (fileGroup dir) files)

fileGroup :: FilePath -> FilePath -> TestTree
fileGroup dir name = testGroup name
  [ testCase "decodes" $ do
      _ <- JSON.decodeFile path
      pure ()
  , testCase "decode . encode . decode == decode" $ do
      v1 <- JSON.decodeFile path
      case JSON.eitherDecode (JSON.encode v1) of
        Left e   -> assertFailure ("re-decode failed: " <> e)
        Right v2 -> v2 @?= v1
  , testCase "encoded form matches the source JSON (modulo key order)" $ do
      bs <- LBS.readFile path
      v  <- JSON.decodeFile path
      let normalize :: LBS.ByteString -> Either String Aeson.Value
          normalize = Aeson.eitherDecode
      normalize (JSON.encode v) @?= normalize bs
  ]
  where
    path = dir </> name
