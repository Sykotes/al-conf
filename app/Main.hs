module Main (main) where

import ParseArgs (parseArgs)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  parseArgs args
