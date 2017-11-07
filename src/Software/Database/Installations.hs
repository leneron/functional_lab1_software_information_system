module Software.Database.Installations (
    create, read, update, delete,
    Id
  ) where

--Module to implement CRUD for installations table

import Prelude hiding (read)
import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import Data.Word
import qualified Software.Database.Computers as Computers
import qualified Software.Database.Programs as Programs

type Id = Word32

create :: IConnection a => Computers.Id -> Programs.Id -> a -> IO Bool
create computer_id program_id conn =
  withTransaction conn (create' computer_id program_id)

create' computer_id program_id conn = do
  changed <- run conn query [SqlWord32 computer_id, SqlWord32 program_id]
  return $ changed == 1
  where
    query = "insert into installations (computer_id, program_id) " ++
            "values (?, ?)"

read :: IConnection a => a -> IO [(Id, Computers.Id, Programs.Id)]
read conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select id, computer_id, program_id from installations order by id"
    unpack [SqlWord32 id, SqlWord32 computer_id, SqlWord32 program_id] =
           (id, computer_id, program_id)
    unpack x = error $ "Error: " ++ show x

update :: IConnection a => Id -> Computers.Id -> Programs.Id -> a -> IO Bool
update id computer_id program_id conn =
  withTransaction conn (update' id computer_id program_id)

update' id computer_id program_id conn = do
  changed <- run conn query
                 [SqlWord32 computer_id, SqlWord32 program_id, SqlWord32 id]
  return $ changed == 1
  where
    query = "update installations set computer_id = ?, program_id = ? " ++
            "where id = ?"

delete :: IConnection a => Id -> a -> IO Bool
delete id conn =
  withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn "delete from installations where id = ?"
                 [SqlWord32 id]
  return $ changed == 1
