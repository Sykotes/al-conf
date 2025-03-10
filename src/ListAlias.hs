module ListAlias
  ( listAlias,
  )
where

import Control.Exception

listAlias :: String -> IO ()
listAlias file_path = do
  result <- try (readFile file_path) :: IO (Either IOError String)
  case result of
    Left err -> do
      putStrLn $ "Failed to read aliases -> ERROR: " ++ show err
      putStrLn "Maybe you are using a different shell"
    Right content -> do
      let file_lines = lines content
      let alias_lines = getAliasLines file_lines []
      let pretty_aliases = prettyAliases alias_lines []
      putStr pretty_aliases

getAliasLines :: [String] -> [String] -> [String]
getAliasLines file_lines found_lines =
  found_lines ++ filter (\line -> take 5 line == "alias") file_lines

prettyAliases :: [String] -> String -> String
prettyAliases alias_lines result
  | null alias_lines = result
  | otherwise = prettyAliases (tail alias_lines) (result ++ head alias_lines ++ "\n")
