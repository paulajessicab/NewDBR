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

mainloop newRepo = do input <- readline prompt
                      case input of
                            Just c -> do addHistory c --historial de comandos
                                         if (c == "generate")
                                         then do generate newRepo
                                                 mainloop newRepo
                                         else do newRepo' <- parseCmd (words c) newRepo
                                                 mainloop newRepo'
                                         where name = get_title newRepo
                            Nothing -> exitWith (ExitFailure 1) ---ver error


                 
{-////////////////| Parser de Comandos |\\\\\\\\\\\\\\\\-}                     
--Cambiar formato
parseCmd :: [String] -> Repo -> IO Repo
--parseCmd = undefined
parseCmd ("title":newttl) repo = return (newtitle (unwords newttl) repo)
parseCmd ("query":newquery) repo = if (h == "SELECT" || h == "select")
                                   then do return (content (unwords newquery) repo)
                                   else do print "--Consulta invalida--"
                                           return repo
                                        where h = head newquery
parseCmd ["exit"] repo = do disconnect (get_connection repo)
                            exitWith ExitSuccess
parseCmd _ repo = do print "Comando no conocido"
                     return repo
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


{-////////////////| Parsers Auxiliares |\\\\\\\\\\\\\\\\-}    
   
-- Parser para tamaños de papel
-- TODO: Tamaños arbitrarios con GPaperHeight y GPaperWidth
parsePageSize :: String -> Maybe PaperType
parsePageSize sz = case (map toLower sz) of
                      {- "a0" -> Just A0	 
                       "a1" -> Just A1	 
                       "a2" -> Just A2
                       "a3" -> Just A3	 
                       "a4" -> Just A4	 
                       "a5" -> Just A5	 
                       "a6" -> Just A6	 
                       "b0" -> Just B0	 
                       "b1" -> Just B1	 
                       "b2" -> Just B2	 
                       "b3" -> Just B3	 
                       "b4" -> Just B4	 
                       "b5" -> Just B5	 
                       "b6" -> Just B6	 
                       "letter" -> Just Letter	 
                       "executive" -> Just Executive	 
                       "legal" -> Just Legal-}
                       otherwise -> Nothing

{-\\\\\\\\\\\\\\\\\\\\\\\///////////////////////-}

{-
--Font Parser VER
parseFont :: String -> Font
parseFont "Arial" = Arial
parseFont "arial" = Arial
parseFont "Times" = Times
parseFont "times" = Times

--Position Parser VER
parsePosition :: String -> Position
parsePosition _ = PCenter
-}
