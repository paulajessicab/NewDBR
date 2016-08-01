import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe

nQuickQuery :: IConnection conn => conn -> String -> [SqlValue] -> IO [[Maybe String]])
nQuickQuery conn qrystr args =
    do sth   <- prepare conn qrystr
       _     <- execute sth args
       res   <- sfetchAllRows' sth
       names <- getColumnNames
       return $ (map (\str -> Just str) names):res
