module Software.Cli.Handlers.Read (
  readEntriesBuilds, readEntriesComputers,
  readEntriesInstallations, readEntriesPrograms,
  readEntriesUsers, readEntriesStatistics
) where

--Read query handlers
--Read the whole tables and display them

import qualified Software.Database.Builds as Builds
import qualified Software.Database.Computers as Computers
import qualified Software.Database.Installations as Installations
import qualified Software.Database.Programs as Programs
import qualified Software.Database.Statistics as Statistics
import qualified Software.Database.Users as Users

import Software.Cli.Printers

readEntriesBuilds conn =
  Builds.read conn >>= mapM_ print >> return True

readEntriesBuilds _         = printError

readEntriesComputers conn =
  Computers.read conn >>= mapM_ print >> return True

readEntriesComputers _      = printError

readEntriesInstallations conn =
  Installations.read conn >>= mapM_ print >> return True

readEntriesInstallations _  = printError

readEntriesPrograms conn =
  Programs.read conn >>= mapM_ print >> return True

readEntriesPrograms _       = printError

readEntriesUsers conn =
  Users.read conn >>= mapM_ print >> return True

readEntriesUsers _          = printError

readEntriesStatistics conn =
  Statistics.read conn >>= mapM_ print >> return True  

readEntriesStatistics _     = printError