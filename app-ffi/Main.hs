module Main (main) where

import Cli qualified
import FFI.Run qualified as Run
import Options.Applicative (execParser)

main :: IO ()
main = do
  opts <- execParser Cli.parserInfo
  Run.run opts
