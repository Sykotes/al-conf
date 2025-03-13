module ListAlias
  ( listAlias,
  )
where

import Control.Exception
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import Help (printHelp)
import System.Console.ANSI

listAlias :: String -> [String] -> IO ()
listAlias filePath other_args = do
  if validOtherArgs other_args
    then do
      result <- try (readFile filePath) :: IO (Either IOException String)
      either handleError processContent result
    else printHelp "list"
  where
    handleError err = do
      putStrLn $ "Failed to read aliases -> ERROR: " ++ show err
      putStrLn "Maybe you are using a different shell"

    processContent content = do
      let valid_options = ['c', 'f']
      let options = getOptions other_args
      if all (`elem` valid_options) options
        then printAliases (getAliasLines content) options
        else printHelp "list"

printAlias :: String -> [Char] -> IO ()
printAlias line options = do
  let _ = options
  let index_eq = indexEq line
  setSGR [Reset]
  if 'f' `elem` options then putStr "Alias " else putStr ""
  if 'c' `elem` options then setSGR [SetColor Foreground Vivid Red] else setSGR [Reset]
  putStr $ take (index_eq - 6) $ drop 6 line
  setSGR [Reset]
  putStr "="
  if 'c' `elem` options then setSGR [SetColor Foreground Vivid Blue] else setSGR [Reset]
  putStrLn $ drop (index_eq + 1) line
  setSGR [Reset]

indexEq :: String -> Int
indexEq line = fromMaybe 0 (elemIndex '=' line)

printAliases :: [String] -> [Char] -> IO ()
printAliases [] _ = putStr ""
printAliases [x] options = printAlias x options
printAliases (x : xs) options = do
  printAlias x options
  printAliases xs options

getAliasLines :: String -> [String]
getAliasLines content = filter ("alias" `isPrefixOf`) $ lines content

getOptions :: [String] -> [Char]
getOptions = foldMap (drop 1)

validOtherArgs :: [String] -> Bool
validOtherArgs = all (\s -> length s >= 2 && head s == '-')
