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
          conn <- parseArg args
          let newRepo = initRepo conn
          hSetBuffering stdout NoBuffering --ver
          mainloop newRepo

--Parser de Argumentos
parseArg :: [String] -> IO Connection
parseArg [database] = connectSqlite3 database
parseArg _ = do putStrLn "-- Error: argumento faltante --"
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
parseInteractive :: [String] -> Repo -> IO Repo
parseInteractive ("load":filename) repo = modify (unwords filename) repo
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
--Salida y error
parseInteractive ["exit"] repo = do disconnect (get_connection repo)
                                    exitWith ExitSuccess
parseInteractive cmd repo =
                   case parse (totParser parseCmd) "Interactive" (unwords cmd) of
                      Left e -> putStrLn (show e) >> return repo
                      Right r -> return $ r repo

printLO :: TableStyle -> IO ()
printLO (a,b,c,d) = do putStr "Exterior:"
                       putStr $ printV a
                       putStr $ printH b
                       putStr "Interior:"
                       putStr $ printV c
                       putStr $ printH d
                     
printV :: Vert -> String
printV None = "Sin bordes verticales"
printV Vert = "Bordes verticales simples"
printV DVert = "Bordes verticales doble"
 
printH :: Bool -> String
printH False = "Sin bordes horizontales"
printH True  = "Con bordes horizontales"
                     
modify :: String -> Repo -> IO Repo
modify filename repo = do file <- readFile filename
                          case parse (totParser parseCommands) filename file of
                              Left e -> putStrLn (show e) >> return repo
                              Right r -> return $ eval r repo

eval :: [(Repo -> Repo)] -> Repo -> Repo
eval [x] repo    = x repo
eval (x:xs) repo = eval xs (x repo) 
{-\\\\\\\\\\\\\\\\\\\\\\\///////////////////////-}
