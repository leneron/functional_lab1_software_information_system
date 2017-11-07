module Software.Cli.Handlers.Update (
	updateEntryBuilds, updateEntryComputers,
  updateEntryInstallations, updateEntryPrograms,
  updateEntryUsers, updateEntryStatistics
) where

--Update query handlers

import qualified Software.Database.Builds as Builds
import qualified Software.Database.Computers as Computers
import qualified Software.Database.Installations as Installations
import qualified Software.Database.Programs as Programs
import qualified Software.Database.Statistics as Statistics
import qualified Software.Database.Users as Users

import Software.Cli.Printers

updateEntryBuilds id (architecture:format:source_url:_) conn =
  Builds.update (read id :: Builds.Id)
        				architecture 
        				format 
        				source_url 
        				conn >>= printResult

updateEntryBuilds _ _ _       = printError

updateEntryComputers id (room:operating_system:vendor:_) conn =
  Computers.update (read id :: Computers.Id)
                   (read room :: Computers.Room) 
                   operating_system
                   vendor
                   conn >>= printResult

updateEntryComputers _ _ _    = printError


updateEntryInstallations id (computer_id:program_id:_) conn =
  Installations.update (read id :: Installations.Id)
                       (read computer_id :: Computers.Id) 
                   	   (read program_id :: Programs.Id) 
                       conn >>= printResult

updateEntryInstallations _ _ _ = printError


updateEntryPrograms id (name:description:publisher:license:build_id:_) conn =
  Programs.update (read id :: Programs.Id)
                  name 
                  description 
                  publisher
                  license
                  (read build_id :: Builds.Id) 
                  conn >>= printResult

updateEntryPrograms _ _ _     = printError

updateEntryUsers id (name:position:_) conn =
  Users.update (read id :: Users.Id)
               name 
               position
               conn >>= printResult

updateEntryUsers _ _ _        = printError

updateEntryStatistics id (user_id:installation_id:exec_time:_) conn =
  Statistics.update (read id :: Statistics.Id)
                    (read user_id :: Users.Id) 
                    (read installation_id :: Installations.Id) 
                    (read exec_time :: Statistics.ExecTime)
                    conn >>= printResult

updateEntryStatistics _ _ _ = printError