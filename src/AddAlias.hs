module AddAlias
  ( addAlias,
  )
where

import Control.Exception
import Data.List (elemIndex)

addAlias :: String -> String -> String -> IO ()
addAlias file_path alias_name alias_cmd = do
  result <- try (readFile file_path) :: IO (Either IOError String)
  case result of
    Left err -> do
      putStrLn $ "Failed to read aliases -> ERROR: " ++ show err
      putStrLn "Maybe you are using a different shell"
    Right content -> do
      let file_lines = lines content
      let alias_lines = getAliasLines file_lines []
      if alias_name `elem` getAliasNames alias_lines
        then
          putStrLn $ "Alias \"" ++ alias_name ++ "\" already exists"
        else do
          let new_alias_line = makeNewLine alias_name alias_cmd
          appendFile file_path new_alias_line

getAliasLines :: [String] -> [String] -> [String]
getAliasLines file_lines found_lines =
  found_lines ++ filter (\line -> take 5 line == "alias") file_lines

getAliasNames :: [String] -> [String]
getAliasNames = map getAliasName

getAliasName :: String -> String
getAliasName alias_line = case elemIndex '=' x of
  Just index_eq -> take index_eq x
  Nothing -> alias_line -- this should never happen
  where
    x = drop 6 alias_line

makeNewLine :: String -> String -> String
makeNewLine alias_name alias_cmd = "\nalias \'" ++ alias_name ++ "\'=\'" ++ alias_cmd ++ "\'\n"
