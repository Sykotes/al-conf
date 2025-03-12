module ParseArgs (parseArgs) where

import AddAlias (addAlias)
import Help (printHelp)
import ListAlias (listAlias)
import RemoveAlias (rmAlias)
import System.Directory (getHomeDirectory)

parseArgs :: [String] -> IO ()
parseArgs args = do
  case validArgs args of
    Just x -> printHelp x
    Nothing -> do
      home <- getHomeDirectory

      let shell_arg = args !! 1
      let file_path = case shell_arg of
            "zsh" -> home ++ "/.zshrc"
            "bash" -> home ++ "/.bashrc"
            _ -> error "Unexpected command" -- Use `error` for unrecoverable conditions
      case head args of
        "add" -> addAlias file_path (args !! 2) (args !! 3)
        "rm" -> rmAlias file_path $ args !! 2
        "list" -> listAlias file_path
        _ -> error "Unexpected command" -- Use `error` for unrecoverable conditions

validArgs :: [String] -> Maybe String
validArgs args =
  case args of
    (cmd : arg : _)
      | cmd `elem` ["add", "rm", "list"],
        arg `elem` ["zsh", "bash", "--help", "-h"] ->
          case cmd of
            "add" -> if length args == 4 && ' ' `notElem` (args !! 2) then Nothing else Just "add"
            "rm" -> if length args == 3 && ' ' `notElem` (args !! 2) then Nothing else Just "rm"
            "list" -> if length args == 2 then Nothing else Just "list"
            _ -> Just "base"
    _ -> Just "base"
