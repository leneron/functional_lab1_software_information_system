module Software.Database.Builds (
    create, read, update, delete,
    Id, Architecture, Format, Source
  ) where

import Prelude hiding (read)
import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import Data.Word

type Id = Word32
type Architecture = String
type Format = String
type Source = String

create :: IConnection a => Architecture -> Format -> Source -> a -> IO Bool
create architecture format source conn =
  withTransaction conn (create' architecture format source)

create' architecture format source conn = do
  changed <- run conn query 
                 [SqlString architecture, SqlString format, SqlString source]
  return $ changed == 1
  where
    query = "insert into builds (architecture, format, source) " ++
            "values (?, ?, ?)"
    
read :: IConnection a => a -> IO [(Id, Architecture, Format, Source)]
read conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select id, architecture, format, source from builds order by id"
    unpack [SqlWord32 id, SqlByteString architecture, SqlByteString format, SqlByteString source] =
           (id, BS.unpack architecture, BS.unpack format, BS.unpack source)
    unpack x = error $ "Error: " ++ show x

update :: IConnection a => Id -> Architecture -> Format -> Source -> a -> IO Bool
update id architecture format source conn =
  withTransaction conn (update' id architecture format source)

update' id architecture format source conn = do
  changed <- run conn query
                 [SqlString architecture, SqlString format, SqlString source, SqlWord32 id]
  return $ changed == 1
  where
    query = "update builds set architecture = ?, format = ?, source = ? " ++
            "where id = ?"

delete :: IConnection a => Id -> a -> IO Bool
delete id conn =
  withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn "delete from builds where id = ?"
                 [SqlWord32 id]
  return $ changed == 1
