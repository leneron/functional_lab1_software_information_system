module Software.Cli.Handlers.Search (
	searchUserHistory, searchRoom, searchProgramTop
) where

import qualified Software.Database.Computers as Computers
import qualified Software.Database.Statistics as Statistics
import qualified Software.Database.Search as Search

import Software.Cli.Printers

--Functions to handle the incoming commands and call query module

searchUserHistory name conn =
  Search.searchUserHistory name conn >>= mapM_ print >> return True

searchUserHistory _ _     = printError

searchRoom room conn =
  Search.searchRoom (read room :: Computers.Room) conn >>= mapM_ print >> return True

searchRoom _ _     = printError

searchProgramTop (start, end) conn =
  Search.searchProgramTop (read start :: Statistics.ExecTime)
                          (read end :: Statistics.ExecTime)
                          conn >>= mapM_ print >> return True

searchProgramTop _ _     = printError