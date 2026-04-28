module Main (main) where

import qualified DearBindings.JSONSpec
import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  jsonTests <- DearBindings.JSONSpec.tests
  defaultMain $ testGroup "dear-bindings-aeson"
    [ jsonTests
    ]
