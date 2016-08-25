module Extra where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe

--Versión modificada de quickQuery para incluir los nombres de columnas

nQuickQuery :: IConnection conn => conn -> String -> [SqlValue] -> IO (Maybe [[Maybe String]])
nQuickQuery conn qrystr args =
    do sth   <- prepare conn qrystr
       _     <- execute sth args
       res   <- sFetchAllRows' sth
       names <- getColumnNames sth
       return $ Just $ (map (\str -> Just str) names):res
       
