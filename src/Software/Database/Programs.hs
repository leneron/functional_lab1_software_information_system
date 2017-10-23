module Software.Database.Programs (
    create, read, update, delete,
    Id, Name, Description, Publisher, License
  ) where

import Prelude hiding (read)
import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import Data.Word
import qualified Software.Database.Builds as Builds

type Id = Word32
type Name = String
type Description = String
type Publisher = String
type License = String

create :: IConnection a => Name -> Description -> Publisher -> License -> Builds.Id -> a -> IO Bool
create name description publisher license build_id conn =
  withTransaction conn (create' name description publisher license build_id)

create' name description publisher license build_id conn = do
  changed <- run conn query [SqlString name, SqlString description, 
                             SqlString publisher, SqlString license, SqlWord32 build_id]
  return $ changed == 1
  where
    query = "insert into programs (name, description, publisher, license, build_id) " ++
            "values (?, ?, ?, ?, ?)"
    
read :: IConnection a => a -> IO [(Id, Name, Description, Publisher, License, Builds.Id)]
read conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select id, name, description, publisher, license, build_id from programs order by id"
    unpack [SqlWord32 id, SqlByteString name, SqlByteString description, 
            SqlByteString publisher, SqlByteString license, SqlWord32 build_id] =
      (id, BS.unpack name, BS.unpack description, 
           BS.unpack publisher, BS.unpack license, build_id)
    unpack x = error $ "Error: " ++ show x

update :: IConnection a => Id -> Name -> Description -> Publisher -> License -> Builds.Id -> a -> IO Bool
update id name description publisher license build_id conn =
  withTransaction conn (update' id name description publisher license build_id)

update' id name description publisher license build_id conn = do
  changed <- run conn query
                 [SqlString name, SqlString description, SqlString publisher, 
                  SqlString license, SqlWord32 build_id, SqlWord32 id]
  return $ changed == 1
  where
    query = "update programs set name = ?, description = ?, " ++
            "publisher = ?, license = ?, build_id = ? " ++
            "where id = ?"

delete :: IConnection a => Id -> a -> IO Bool
delete id conn =
  withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn "delete from programs where id = ?"
                 [SqlWord32 id]
  return $ changed == 1
