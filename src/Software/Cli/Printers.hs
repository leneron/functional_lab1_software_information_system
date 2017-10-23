module Software.Cli.Printers (
	printResult, quit, printHelp, printError
) where

import System.IO

printResult :: Bool -> IO Bool
printResult True  = putStrLn "Success" >> return True
printResult False = putStrLn "Failure" >> return True

quit :: IO Bool
quit = do
  putStrLn "quit..."
  return False

printHelp :: IO Bool
printHelp = do
  mapM_ putStrLn help
  return True

printError :: IO Bool
printError = do
  putStrLn "Error happened, use 'help' command"
  return True

help :: [String]
help =
  ["Separate your fields with space. Text in quotes is considered as a single field\n"
  ,"create <table>        <params> - create entry in the table"
  ,"create builds         <architecture>  <format>  <source_url>"
  ,"create computers      <room>  <operating_system>  <vendor>"
  ,"create installations  <computer_id>  <program_id>"
  ,"create programs       <name>  <description>  <publisher>  <license>  <build_id>"
  ,"create statistics     <user_id>  <installation_id> <exec_time[YYYY-MM-DD HH:MM:SS]>"
  ,"create users          <name>  <student|assistant|professor|management|other>\n"
  ,"read   <table> - read all data from table"
  ,"read   builds"
  ,"read   computers"
  ,"read   installations"
  ,"read   programs"
  ,"read   statistics"
  ,"read   users\n"
  ,"update <table>       <id>   <params> - update entry in the table with specified id"
  ,"update builds        <id>   <architecture>  <format>  <source_url>"
  ,"update computers     <id>   <room>  <operating_system>  <vendor>"
  ,"update installations <id>   <computer_id>  <program_id>"
  ,"update programs      <id>   <name>  <description>  <publisher>  <license>  <build_id>"
  ,"update statistics    <id>   <user_id>  <installation_id>  <exec_time[YYYY-MM-DD HH:MM:SS]>"
  ,"update users         <id>   <name>  <student|assistant|professor|management|other>\n"
  ,"delete <table> <id> - delete entry with specified id"
  ,"delete builds        <id>"
  ,"delete computers     <id>"
  ,"delete installations <id>"
  ,"delete programs      <id>"
  ,"delete statistics    <id>"
  ,"delete users         <id>\n"
  ,"search <params> -- search available info about"
  ,"search user history <user_name>"
  ,"search room <room>"
  ,"search program top <start[YYYY-MM-DD HH:MM:SS]> <end[YYYY-MM-DD HH:MM:SS]>\n"
  ,"quit               - quit"
  ,"help               - help"
  ]