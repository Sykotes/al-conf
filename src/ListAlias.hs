module ListAlias
  ( listAlias,
  )
where

import Control.Exception
import Data.List (isPrefixOf)
import Help (printHelp)

listAlias :: String -> [String] -> IO ()
listAlias filePath other_args = do
  print $ validOtherArgs other_args
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
      print options
      if all (`elem` valid_options) options
        then putStr $ prettyAliases (getAliasLines $ lines content) options
        else printHelp "list"

prettyAliases :: [String] -> [Char] -> String
prettyAliases [] _ = ""
prettyAliases [x] _ = x ++ "\n"
prettyAliases (x : xs) y = x ++ "\n" ++ prettyAliases xs y

getAliasLines :: [String] -> [String]
getAliasLines = filter ("alias" `isPrefixOf`)

getOptions :: [String] -> [Char]
getOptions = foldMap (drop 1)

validOtherArgs :: [String] -> Bool
validOtherArgs = all (\s -> length s >= 2 && head s == '-')
