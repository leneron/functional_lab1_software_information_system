module Software.Database.Search (
  searchUserHistory, searchRoom, searchProgramTop
) where

import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import Data.Int

import qualified Software.Database.Builds as Builds
import qualified Software.Database.Computers as Computers
import qualified Software.Database.Installations as Installations
import qualified Software.Database.Programs as Programs
import qualified Software.Database.Statistics as Statistics
import qualified Software.Database.Users as Users

--Searching the user history of programs usage
searchUserHistory :: IConnection a => Users.Name -> a -> IO [(Statistics.Id, Users.Name, 
                                                              Programs.Name, Statistics.ExecTime)]
searchUserHistory name conn = do
  result <- quickQuery' conn query [SqlString name]
  return $ map unpack result
  where
    query = "select s.id, u.name, p.name, s.exec_time " ++ 
            "from statistics s " ++ 
            "inner join users u on s.user_id = u.id " ++
            "inner join installations i on s.installation_id = i.id " ++
            "inner join programs p on i.program_id = p.id " ++
            "where u.name = ? " ++
            "order by s.exec_time"
    unpack [SqlWord32 id, SqlByteString user_name, SqlByteString program_name, SqlLocalTime exec_time] =
           (id, BS.unpack user_name, BS.unpack program_name, exec_time)
    unpack x = error $ "Error: " ++ show x

--Search the existing software on the computers in the room
searchRoom :: IConnection a => Computers.Room -> a -> IO [(Computers.Id, Computers.OperatingSystem, Computers.Vendor, 
                                                           Programs.Name, Builds.Architecture, Builds.Format)]
searchRoom room conn = do
  result <- quickQuery' conn query [SqlWord32 room]
  return $ map unpack result
  where
    query = "select c.id, c.operating_system, c.vendor, p.name, b.architecture, b.format " ++ 
            "from computers c " ++ 
            "inner join installations i on c.id = i.computer_id " ++
            "inner join programs p on i.program_id = p.id " ++
            "inner join builds b on p.build_id = b.id " ++
            "where c.room = ? " ++
            "order by c.id"
    unpack [SqlWord32 id, SqlByteString operating_system, SqlByteString vendor, 
            SqlByteString program_name, SqlByteString architecture, SqlByteString format] =
           (id, BS.unpack operating_system, BS.unpack vendor, 
            BS.unpack program_name, BS.unpack architecture, BS.unpack format)
    unpack x = error $ "Error: " ++ show x

--Search top-used programs between the dates
searchProgramTop :: IConnection a => Statistics.ExecTime -> Statistics.ExecTime -> a -> IO [(Programs.Name, Int64)]
searchProgramTop start end conn = do
  result <- quickQuery' conn query [SqlLocalTime start, SqlLocalTime end]
  return $ map unpack result
  where
    query = "select p.name, count(s.exec_time) " ++ 
            "from statistics s " ++ 
            "inner join installations i on s.installation_id = i.id " ++
            "inner join programs p on i.program_id = p.id " ++
            "where s.exec_time between ? and ? " ++
            "group by p.name " ++
            "order by count(s.exec_time)"
    unpack [SqlByteString name, SqlInt64 count] =
           (BS.unpack name, count)
    unpack x = error $ "Error: " ++ show x
