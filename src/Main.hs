module Main(

  module AST,
  module Parser,
  module DBReport,
  main

) where

import AST
import Parser
import DBReport
--sistema
import System.Environment(getArgs)
import System.IO  
import System.Exit
import System.Console.Readline(readline,addHistory)
import System.Directory(doesFileExist)
--sqlite
import Database.HDBC(disconnect)
import Database.HDBC.Sqlite3(Connection, connectSqlite3)
--parser
import Text.ParserCombinators.Parsec(parse)

import Control.Monad (unless)

--Fuentes
--main interactivo de http://learnyouahaskell.com/input-and-output NO
--https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling NO
-- TP lis
--https://en.wikibooks.org/wiki/LaTeX/Tables
-- documento geometry
-- http://hackage.haskell.org/package/HaTeX-3.17.0.2/docs/src/Text-LaTeX-Base-Commands.html#ClassOption  -- VER: Special Tables

main::IO()
main = do args <- getArgs
          conn <- parseArg args
          let newRepo = initRepo conn
          hSetBuffering stdout NoBuffering
          mainloop newRepo

--Parser de Argumentos
parseArg :: [String] -> IO Connection
parseArg []  = do putStrLn "<- DBR Error: argumento faltante ->"
                  putStrLn "<- DBR Uso: ./Main [database]    ->"
                  exitWith (ExitFailure 1)
parseArg dbs = do fileExists <- doesFileExist database
                  if fileExists
                    then do conn <- connectSqlite3 database
                            return conn
                    else do putStrLn "<-DBR Error: DB Inexistente ->"
                            exitWith (ExitFailure 1)
                       where database = unwords dbs

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
                            Nothing -> exitWith (ExitFailure 1)
                 
{-////////////////| Parser de Comandos Interactivos |\\\\\\\\\\\\\\\\-}                
parseInteractive :: [String] -> Repo -> IO Repo
parseInteractive ("load":filename) repo = do 
                    fileExists <- doesFileExist fname
                    if fileExists
                      then do repo' <- modify fname repo
                              return repo'
                      else do putStrLn "<-DBR Error: Archivo Inexistente ->"
                              return repo
                        where fname = unwords filename
--Obtener datos
parseInteractive ["show","title"] repo = do print $ get_title repo
                                            return repo
parseInteractive ["show","title","font"] repo = do
                                    print $ (show $ get_title_font repo)
                                             ++ " " ++
                                            (show $ get_title_pos repo)
                                    return repo
parseInteractive ["show","query"] repo = do print (get_query repo)
                                            return repo
parseInteractive ["show","table", "layout"] repo = do printLO $ get_layout repo                                             
                                                      return repo
parseInteractive ["show","table","header","font"] repo = do
                                            print $ show $ get_table_hfont repo
                                            return repo
parseInteractive ["show", "table","body","font"] repo = do 
                                            print $ show $ get_table_bfont repo
                                            return repo
parseInteractive ["show","paper","type"] repo = do
                                             print $ show $ get_paper_size repo
                                             return repo
parseInteractive ["show","paper","landscape","status"] repo = do
                                              print $ get_paper_lands repo
                                              return repo
parseInteractive ["show","paper","title","status"] repo = do
                                              print $ get_paper_title repo
                                              return repo
--Salida
parseInteractive ["exit"] repo = do disconnect (get_connection repo)
                                    exitWith ExitSuccess
--Modificacion o error
parseInteractive cmd repo =
                   case parse (totParser parseCmd') "Interactive" (unwords cmd) of
                      Left e -> putStrLn (show e) >> return repo
                      Right r -> return $ r repo

{-/////////////////////////| Auxiliares |\\\\\\\\\\\\\\\\\\\\\\\\\-}
--Mini pretty printing
printLO :: TableStyle -> IO ()
printLO (a,b,c,d) = do putStr "Exterior:\n"
                       putStr $ printV a
                       putStr $ printH b
                       putStr "Interior:\n"
                       putStr $ printV c
                       putStr $ printH d
                     
printV :: Vert -> String
printV None = "Sin bordes verticales\n"
printV Vert = "Bordes verticales simples\n"
printV DVert = "Bordes verticales doble\n"
 
printH :: Bool -> String
printH False = "Sin bordes horizontales\n"
printH True  = "Con bordes horizontales\n"

--Parseo desde archivo                     
modify :: String -> Repo -> IO Repo
modify filename repo = do file <- readFile filename
                          case parse (totParser parseCommands) filename file of
                              Left e -> putStrLn (show e) >> return repo
                              Right r -> return $ eval r repo

--Aplicacion de las funciones
eval :: [(Repo -> Repo)] -> Repo -> Repo
eval [x] repo    = x repo
eval (x:xs) repo = eval xs (x repo) 
{-\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////-}
