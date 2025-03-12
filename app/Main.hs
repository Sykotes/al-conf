module Main (main) where

import AddAlias (addAlias)
import Control.Exception
import Control.Monad (unless)
import ListAlias (listAlias)
import RemoveAlias (rmAlias)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  home <- getHomeDirectory

  let help_strings = ["-h", "--help"]
  let cmd_strings = ["add", "rm", "list"]
  let shell_strings = ["zsh", "bash"]

  case args of
    [arg] | arg `elem` help_strings -> do
      printHelp "base"
    [arg, arg2] | arg `elem` cmd_strings && arg2 `elem` help_strings -> printHelp arg
    _ ->
      if validArgs args
        then do
          let shell_arg = args !! 1
          unless (shell_arg `elem` shell_strings) $ do
            putStrLn "Invalid command"
            exitFailure
          let file_path = case shell_arg of
                "zsh" -> home ++ "/.zshrc"
                "bash" -> home ++ "/.bashrc"
                _ -> error "Unexpected command" -- Use `error` for unrecoverable conditions
          case head args of
            "add" -> addAlias file_path (args !! 2) (args !! 3)
            "rm" -> rmAlias file_path $ args !! 2
            "list" -> listAlias file_path
            _ -> error "Unexpected command" -- Use `error` for unrecoverable conditions
        else printHelp "base"

validArgs :: [String] -> Bool
validArgs args =
  case args of
    (cmd : arg : _)
      | cmd `elem` ["add", "rm", "list"],
        arg `elem` ["zsh", "bash", "--help"] ->
          case cmd of
            "add" -> length args == 4 && ' ' `notElem` (args !! 2)
            "rm" -> length args == 3 && ' ' `notElem` (args !! 2)
            "list" -> length args == 2
            _ -> False
    _ -> False

printHelp :: String -> IO ()
printHelp help_type = do
  let help_types = ["base", "add", "rm", "list", "invalid"]
  assert (help_type `elem` help_types) $
    case help_type of
      "base" ->
        putStrLn $
          unlines
            [ "Usage: al-conf <command> [options]",
              "Commands:",
              "  add   <shell> <alias> <command>   Add a new alias",
              "  rm    <shell> <alias>            Remove an alias",
              "  list  <shell>                    List all aliases",
              "  -h, --help                       Show this help message",
              "",
              "Supported shells: bash, zsh",
              "Use 'al-conf <command> --help' for more details on a command."
            ]
      "add" ->
        putStrLn $
          unlines
            [ "Usage: al-conf add <shell> <alias> <command>",
              "Adds a new alias to the specified shell configuration file.",
              "",
              "Arguments:",
              "  <shell>   The shell type (bash or zsh)",
              "  <alias>   The name of the alias (no spaces allowed)",
              "  <command> The command the alias should execute",
              "",
              "Example:",
              "  al-conf add zsh ll 'ls -la'"
            ]
      "rm" ->
        putStrLn $
          unlines
            [ "Usage: al-conf rm <shell> <alias>",
              "Removes an alias from the specified shell configuration file.",
              "",
              "Arguments:",
              "  <shell>   The shell type (bash or zsh)",
              "  <alias>   The name of the alias to remove",
              "",
              "Example:",
              "  al-conf rm bash ll"
            ]
      "list" ->
        putStrLn $
          unlines
            [ "Usage: al-conf list <shell>",
              "Lists all aliases currently defined in the specified shell configuration file.",
              "",
              "Arguments:",
              "  <shell>   The shell type (bash or zsh)",
              "",
              "Example:",
              "  al-conf list zsh"
            ]
      "invalid" -> putStrLn "Invalid command. Use 'al-conf --help' for usage information."
      _ -> error "Unhandled help_type"
