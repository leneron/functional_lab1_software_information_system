module Software.Database.Computers (
    create, read, update, delete,
    Id, Room, OperatingSystem, Vendor
  ) where

import Prelude hiding (read)
import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import Data.Word

type Id = Word32
type Room = Word32
type OperatingSystem = String
type Vendor = String

create :: IConnection a => Room -> OperatingSystem -> Vendor -> a -> IO Bool
create room operating_system vendor conn = 
  withTransaction conn (create' room operating_system vendor)

create' room operating_system vendor conn = do
  changed <- run conn query 
                 [SqlWord32 room, SqlString operating_system, SqlString vendor]
  return $ changed == 1
  where
    query = "insert into computers (room, operating_system, vendor)" ++
            " values (?, ?, ?)"
    
read :: IConnection a => a -> IO [(Id, Room, OperatingSystem, Vendor)]
read conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select id, room, operating_system, vendor from computers order by id"
    unpack [SqlWord32 id, SqlWord32 room, SqlByteString operating_system, SqlByteString vendor] =
           (id, room, BS.unpack operating_system, BS.unpack vendor)
    unpack x = error $ "Error: " ++ show x

update :: IConnection a => Id -> Room -> OperatingSystem -> Vendor -> a -> IO Bool
update id room operating_system vendor conn =
  withTransaction conn (update' id room operating_system vendor)

update' id room operating_system vendor conn = do
  changed <- run conn query
                 [SqlWord32 room, SqlString operating_system, SqlString vendor, SqlWord32 id]
  return $ changed == 1
  where
    query = "update computers set room = ?, operating_system = ?, vendor = ? " ++
            "where id = ?"

delete :: IConnection a => Id -> a -> IO Bool
delete id conn =
  withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn "delete from computers where id = ?"
                 [SqlWord32 id]
  return $ changed == 1
