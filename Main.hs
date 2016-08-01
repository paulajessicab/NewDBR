module Main where

import DBReport
import AST
import System.Environment   
import System.Directory  
import System.IO  
import Data.List
import System.Exit
import Control.Monad
import Database.HDBC.Sqlite3
import Database.HDBC
import System.Console.Readline --ver

--main interactivo de http://learnyouahaskell.com/input-and-output NO
--https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling NO
-- TP lis
--Ver Errores

           
main = do args <- getArgs
          conn <- parse' args
          let newRepo = initRepo conn
          mainloop newRepo

--Parser de Argumentos

parse' :: [String] -> IO Connection
parse' [database] = connectSqlite3 database
parse' _ = do putStrLn "-- Error: argumento faltante --"
              putStrLn "Uso: ./Main [database]"
              exitWith (ExitFailure 1)

--Intérprete de Comandos

prompt :: String
prompt = ">> "

mainloop newRepo = do input <- readline prompt
                      case input of
                            Just c -> do addHistory c --historial de comandos
                                         if (c == "generate")
                                         then do generate newRepo
                                                 mainloop newRepo
                                         else do newRepo' <- parseCmd (words c) newRepo
                                                 mainloop newRepo'
                            Nothing -> exitWith (ExitFailure 1) ---ver error


                 
--Parser de Comandos                      

parseCmd :: [String] -> Repo -> IO Repo
parseCmd ("title":newttl) repo = return (title (unwords newttl) repo)
parseCmd ("query":newquery) repo = if (h == "SELECT" || h == "select")
                                   then do return (content (unwords newquery) repo)
                                   else do print "--Consulta invalida--" 
                                           return repo
                                        where h = head newquery
{-}parseCmd ["show", "title"] repo = do print (get_title repo)
                                     return repo
parseCmd ["show", "title", "style"] repo = do print (get_title_stl repo)
                                              return repo
parseCmd ["show", "body", "style"] repo = do print (get_body_stl repo)
                                             return repo
parseCmd ["show", "columns"] repo = do print (get_columns repo)
                                       return repo -}
parseCmd ["exit"] repo = do disconnect (get_connection repo)
                            exitWith ExitSuccess
parseCmd _ repo = do print "Comando no conocido"--exitWith (ExitFailure 1) --error y seguir
                     return repo

--Font Parser VER
{-parseFont :: String -> Font
parseFont "Arial" = Arial
parseFont "arial" = Arial
parseFont "Times" = Times
parseFont "times" = Times
-}
--Position Parser VER
parsePosition :: String -> Position
parsePosition _ = PCenter
