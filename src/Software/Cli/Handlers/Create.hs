module Software.Cli.Handlers.Create (
	createEntryBuilds, createEntryComputers,
  createEntryInstallations, createEntryPrograms,
  createEntryUsers, createEntryStatistics
) where

import qualified Software.Database.Builds as Builds
import qualified Software.Database.Computers as Computers
import qualified Software.Database.Installations as Installations
import qualified Software.Database.Programs as Programs
import qualified Software.Database.Statistics as Statistics
import qualified Software.Database.Users as Users

import Software.Cli.Printers

createEntryBuilds (architecture:format:source_url:_) conn =
  Builds.create architecture 
        				format 
        				source_url 
        				conn >>= printResult

createEntryBuilds _ _        = printError

createEntryComputers (room:operating_system:vendor:_) conn =
  Computers.create (read room :: Computers.Room) 
                    operating_system
                    vendor
            				conn >>= printResult

createEntryComputers _ _     = printError

createEntryInstallations (computer_id:program_id:_) conn =
  Installations.create (read computer_id :: Computers.Id) 
          					   (read program_id :: Programs.Id) 
          					   conn >>= printResult

createEntryInstallations _ _ = printError

createEntryPrograms (name:description:publisher:license:build_id:_) conn =
  Programs.create name 
                  description 
                  publisher
                  license
                  (read build_id :: Builds.Id) 
                  conn >>= printResult

createEntryPrograms _ _      = printError

createEntryUsers (name:position:_) conn =
  Users.create name 
               position
               conn >>= printResult

createEntryUsers _ _         = printError

createEntryStatistics (user_id:installation_id:exec_time:_) conn =
  Statistics.create (read user_id :: Users.Id) 
                    (read installation_id :: Installations.Id) 
                    (read exec_time :: Statistics.ExecTime)
                    conn >>= printResult

createEntryStatistics _ _    = printError