module Software.Database.Users (
    create, read, update, delete, --positionFromStr,
    Id, Name, Position
  ) where

import Prelude hiding (read)
import Database.HDBC
import qualified Data.ByteString.Char8 as BS
import Data.Word

type Id = Word32
type Name = String
type Position = String

-- positionFromStr :: String -> Position
-- positionFromStr "student" = Student
-- positionFromStr "assistant" = Assistant
-- positionFromStr "professor" = Professor
-- positionFromStr "management" = Management
-- positionFromStr "other" = Other

-- strFromPosition :: Position -> String
-- strFromPosition Student = "student"
-- strFromPosition Assistant = "assistant"
-- strFromPosition Professor = "professor"
-- strFromPosition Management = "management"
-- strFromPosition Other = "other"


create :: IConnection a => Name -> Position -> a -> IO Bool
create name position conn =
  withTransaction conn (create' name position)

create' name position conn = do
  changed <- run conn query [SqlString name, SqlString position]
  return $ changed == 1
  where
    query = "insert into users (name, position) " ++
            "values (?, ?)"
    
read :: IConnection a => a -> IO [(Id, Name, Position)]
read conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select id, name, position from users order by id"
    unpack [SqlWord32 id, SqlByteString name, SqlByteString position] =
      (id, BS.unpack name, BS.unpack position)
    unpack x = error $ "Error: " ++ show x

update :: IConnection a => Id -> Name -> Position -> a -> IO Bool
update id name position conn =
  withTransaction conn (update' id name position)

update' id name position conn = do
  changed <- run conn query
                 [SqlString name, SqlString position, SqlWord32 id]
  return $ changed == 1
  where
    query = "update users set name = ?, position = ? " ++
            "where id = ?"

delete :: IConnection a => Id -> a -> IO Bool
delete id conn =
  withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn "delete from users where id = ?"
                 [SqlWord32 id]
  return $ changed == 1
