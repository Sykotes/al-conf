module RemoveAlias
  ( rmAlias,
  )
where

import Control.Exception

rmAlias :: String -> String -> IO ()
rmAlias file_path alias_name = do
  result <- try (readFile file_path) :: IO (Either IOError String)
  case result of
    Left err -> do
      putStrLn $ "Failed to read aliases -> ERROR: " ++ show err
      putStrLn "Maybe you are using a different shell"
    Right content -> do
      let file_lines = lines content
      let new_file_lines = getNewFileLines file_lines alias_name

      if new_file_lines == map (++ "\n") file_lines
        then putStrLn $ "Alias \"" ++ alias_name ++ "\" not found"
        else do
          let new_contents = concat new_file_lines
          writeFile file_path new_contents
          putStrLn $ "Alias \"" ++ alias_name ++ "\" removed"

-- accumulate each line unless it is the alias named
emptyIfAlias :: String -> String -> String
emptyIfAlias file_line alias_name
  | take 6 file_line ++ alias_name == take (6 + length alias_name) file_line = ""
  | otherwise = file_line ++ "\n"

getNewFileLines :: [String] -> String -> [String]
getNewFileLines file_lines alias_name = foldl (\acc x -> acc ++ [emptyIfAlias x alias_name]) [] file_lines
