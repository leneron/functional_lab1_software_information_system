module Software.Cli.Cli (main) where

import Software.Cli.Handlers.Create
import Software.Cli.Handlers.Read
import Software.Cli.Handlers.Update
import Software.Cli.Handlers.Delete
import Software.Cli.Handlers.Search

import System.IO
import Control.Monad (when)
import Data.Pool

import Software.Cli.Printers
import TextUtils

--Module for splitting and recognition the input command name 
--and passing the parameters to the appropriate handlers

main pool = do
  putStrLn "Enter the command (use 'help' if needed):"
  mainLoop pool

--Run the main loop forever until user interrupts it or quit 
mainLoop pool = do
  putStr "software> "
  hFlush stdout
  line <- getLine
  continue <- withResource pool $ \conn ->  processCommand conn (splitWithQuotes line)
  when continue $ mainLoop pool

--Processing the commands with pattern matching 
processCommand conn ("create":"builds":fields)        = createEntryBuilds fields conn
processCommand conn ("create":"computers":fields)     = createEntryComputers fields conn
processCommand conn ("create":"installations":fields) = createEntryInstallations fields conn
processCommand conn ("create":"programs":fields)      = createEntryPrograms fields conn
processCommand conn ("create":"statistics":fields)    = createEntryStatistics fields conn
processCommand conn ("create":"users":fields)         = createEntryUsers fields conn

processCommand conn ("read":"builds":_)          = readEntriesBuilds conn
processCommand conn ("read":"computers":fields)  = readEntriesComputers conn
processCommand conn ("read":"installations":_)   = readEntriesInstallations conn
processCommand conn ("read":"programs":fields)   = readEntriesPrograms conn
processCommand conn ("read":"statistics":fields) = readEntriesStatistics conn
processCommand conn ("read":"users":fields)      = readEntriesUsers conn

processCommand conn ("update":"builds":id:fields)        = updateEntryBuilds id fields conn
processCommand conn ("update":"computers":id:fields)     = updateEntryComputers id fields conn
processCommand conn ("update":"installations":id:fields) = updateEntryInstallations id fields conn
processCommand conn ("update":"programs":id:fields)      = updateEntryPrograms id fields conn
processCommand conn ("update":"statistics":id:fields)    = updateEntryStatistics id fields conn
processCommand conn ("update":"users":id:fields)         = updateEntryUsers id fields conn


processCommand conn ("delete":"builds":id:_)        = deleteEntryBuilds id conn
processCommand conn ("delete":"computers":id:_)     = deleteEntryComputers id conn
processCommand conn ("delete":"installations":id:_) = deleteEntryInstallations id conn
processCommand conn ("delete":"programs":id:_)      = deleteEntryPrograms id conn
processCommand conn ("delete":"statistics":id:_)    = deleteEntryStatistics id conn
processCommand conn ("delete":"users":id:_)         = deleteEntryUsers id conn

processCommand conn ("search":"user":"history":name:_)     = searchUserHistory name conn
processCommand conn ("search":"room":room:_)               = searchRoom room conn
processCommand conn ("search":"program":"usage":start:end:_) = searchProgramTop (start, end) conn

processCommand _ ("quit":_)                    = quit
processCommand _ ("help":_)                    = printHelp
processCommand _ _                             = printError

