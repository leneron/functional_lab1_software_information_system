module Software.Cli.Handlers.Delete (
  deleteEntryBuilds, deleteEntryComputers,
  deleteEntryInstallations, deleteEntryPrograms,
  deleteEntryUsers, deleteEntryStatistics
) where

--Delete query handlers

import qualified Software.Database.Builds as Builds
import qualified Software.Database.Computers as Computers
import qualified Software.Database.Installations as Installations
import qualified Software.Database.Programs as Programs
import qualified Software.Database.Statistics as Statistics
import qualified Software.Database.Users as Users

import Software.Cli.Printers

deleteEntryBuilds id conn =
  Builds.delete (read id :: Builds.Id) conn >>= printResult

deleteEntryBuilds _ _        = printError

deleteEntryComputers id conn =
  Computers.delete (read id :: Computers.Id) conn >>= printResult

deleteEntryComputers _ _     = printError

deleteEntryInstallations id conn =
  Installations.delete (read id :: Installations.Id) conn >>= printResult

deleteEntryInstallations _ _ = printError

deleteEntryPrograms id conn =
  Programs.delete (read id :: Programs.Id) conn >>= printResult

deleteEntryPrograms _ _      = printError

deleteEntryUsers id conn =
  Users.delete (read id :: Users.Id) conn >>= printResult

deleteEntryUsers _ _         = printError

deleteEntryStatistics id conn =
  Statistics.delete (read id :: Statistics.Id) conn >>= printResult

deleteEntryStatistics _ _    = printError