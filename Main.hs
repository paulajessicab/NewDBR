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
import Data.Char (toLower) 
import System.Process
import Text.LaTeX.Base.Commands(PaperType) --Oculta los valores???
import Data.Either
import Parser
import Text.ParserCombinators.Parsec
--TODO
-- Ver excepciones sqlite!!
-- Usar tablas!!
-- Terminar landscape, margenes y encabezados/pies
-- Sacar salida de pdflatex
-- Ver que pagestyle queda mejor
-- Tamaño de fuentes con relsize?

--Fuentes
--main interactivo de http://learnyouahaskell.com/input-and-output NO
--https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling NO
-- TP lis
--https://en.wikibooks.org/wiki/LaTeX/Tables
-- documento geometry
-- http://hackage.haskell.org/package/HaTeX-3.17.0.2/docs/src/Text-LaTeX-Base-Commands.html#ClassOption  -- VER: Special Tables




main::IO()
main = do args <- getArgs
          conn <- parse' args
          let newRepo = initRepo conn
          hSetBuffering stdout NoBuffering --ver
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

mainloop::Repo->IO ()
mainloop newRepo = do input <- readline prompt
                      case input of
                            Just c -> do addHistory c --historial de comandos
                                         if (c == "generate")
                                         then do generate newRepo
                                                 mainloop newRepo
                                         else do newRepo' <- parseInteractive (words c) newRepo
                                                 mainloop newRepo'
                                         where name = get_title newRepo
                            Nothing -> exitWith (ExitFailure 1) ---ver error


                 
{-////////////////| Parser de Comandos |\\\\\\\\\\\\\\\\-}                     
--Cambiar formato
parseInteractive :: [String] -> Repo -> IO Repo
--parseCmd = undefined
parseInteractive ("load":filename) repo = modify (unwords filename) repo
parseInteractive ("title":newttl) repo = return (titleNew (unwords newttl) repo)
parseInteractive ("query":newquery) repo = if (h == "SELECT" || h == "select")
                                           then do return (query (unwords newquery) repo)
                                           else do print "--Consulta invalida--"
                                                   return repo
                                             where h = head newquery
parseInteractive ["exit"] repo = do disconnect (get_connection repo)
                                    exitWith ExitSuccess
parseInteractive _ repo = do print "Comando no conocido"
                             return repo
                     
                     
                     
modify :: String -> Repo -> IO Repo
modify filename repo = do file <- readFile filename
                          case parse (totParser parseTitle) filename file of
                              Left e -> putStrLn (show e) >> return repo
                              Right r -> return $ eval r repo

eval :: (Repo -> Repo) -> Repo -> Repo
eval p repo = p repo 

{-parseCmd ("pagesize":newsize) repo = case parsePageSize (unwords newsize) of
                                        Just x -> return (pagesize x repo)
                                        Nothing -> do print "--Dimensiones de pagina no validas--"
                                                      print "--Las soportados son ..." --TODO
                                                      return repo
--Obtener datos
parseCmd ["show", "title"] repo = do print (get_title repo)
                                     return repo
--Salida y error
parseCmd ["exit"] repo = do disconnect (get_connection repo)
                            exitWith ExitSuccess
parseCmd _ repo = do print "Comando no conocido"
                     return repo
-}         
--Obtener datos
--parseCmd ["show", "title", "style"] repo = do print (get_title_stl repo)
--                                              return repo
--parseCmd ["show", "body", "style"] repo = do print (get_body_stl repo)
--                                             return repo
--parseCmd ["show", "columns"] repo = do print (get_columns repo)
--                                       return repo 
{-\\\\\\\\\\\\\\\\\\\\\\\///////////////////////-}
