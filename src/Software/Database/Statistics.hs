module Software.Database.Statistics (
    create, read, update, delete,
    Id, ExecTime
  ) where

import Prelude hiding (read)
import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Time.LocalTime
import qualified Software.Database.Installations as Installations
import qualified Software.Database.Users as Users

type Id = Word32
type ExecTime = LocalTime

create :: IConnection a => Users.Id -> Installations.Id -> ExecTime -> a -> IO Bool
create user_id installation_id exec_time conn =
  withTransaction conn (create' user_id installation_id exec_time)

create' user_id installation_id exec_time conn = do
  changed <- run conn query [SqlWord32 user_id, SqlWord32 installation_id, SqlLocalTime exec_time]
  return $ changed == 1
  where
    query = "insert into statistics (user_id, installation_id, exec_time)" ++
            " values (?, ?, ?)"
    
read :: IConnection a => a -> IO [(Id, Users.Id, Installations.Id, ExecTime)]
read conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select id, user_id, installation_id, exec_time from statistics order by id"
    unpack [SqlWord32 id, SqlWord32 user_id, SqlWord32 installation_id, SqlLocalTime exec_time] =
      (id, user_id, installation_id, exec_time)
    unpack x = error $ "Error: " ++ show x

update :: IConnection a => Id -> Users.Id -> Installations.Id -> ExecTime -> a -> IO Bool
update id user_id installation_id exec_time conn =
  withTransaction conn (update' id user_id installation_id exec_time)

update' id user_id installation_id exec_time conn = do
  changed <- run conn query
                 [SqlWord32 user_id, SqlWord32 installation_id, SqlLocalTime exec_time, SqlWord32 id]
  return $ changed == 1
  where
    query = "update statistics set user_id = ?, installation_id = ?, exec_time = ? " ++
            "where id = ?"

delete :: IConnection a => Id -> a -> IO Bool
delete id conn =
  withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn "delete from builds where id = ?"
                 [SqlWord32 id]
  return $ changed == 1
