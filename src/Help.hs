module Help (printHelp) where

printHelp :: String -> IO ()
printHelp help_type = case help_type of
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
  _ -> error $ "Unhandled help_type" ++ help_type
