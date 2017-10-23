module Software.Database.Connection (
    runConnection
  ) where

--Connecting to database and obtaining the pool of connections

import Database.HDBC
import Database.HDBC.MySQL
import Data.Time.Clock
import Data.Pool
import Data.Typeable

runConnection f = do
  (connectInfo, stripes, timeout, connections) <- getConnectionParams
  pool <- createPool (connectMySQL MySQLConnectInfo {
                                    mysqlHost       = "localhost",
                                    mysqlUser       = "root",
                                    mysqlPassword   = "123456",
                                    mysqlPort       = 3306,
                                    mysqlUnixSocket = "/var/lib/mysql/mysql.sock",
                                    mysqlDatabase   = "software_db",
                                    mysqlGroup      = Nothing
                                  }) 
                      disconnect 
                      stripes 
                      timeout 
                      connections
  f pool

getConnectionParams :: IO (String, Int, NominalDiffTime, Int)
getConnectionParams = do
  let 
    host = "localhost"
    port = 3306
    name = "software_db"
    user = "root"
    password = "123456"
    connectInfo = "host=" ++ host 
                  ++ " port=" ++ show port 
                  ++ " dbname=" ++ name 
                  ++ " user=" ++ user 
                  ++ " password=" ++ password
    stripes = 1
    connections = 10
    timeout = 60
  return (connectInfo, stripes, timeout, connections)