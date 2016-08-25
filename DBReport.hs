{-# LANGUAGE OverloadedStrings #-}

module DBReport where

import AST
import Extra
import System.Environment
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.List
import System.IO
import System.Process
--PDF
--import Graphics.PDF
--import Graphics.PDF.Typesetting
--
import Control.Monad.State
import Data.Bool
import Control.Exception
import Data.Maybe
import Data.Matrix
import qualified Data.Text as T
--latex
import Text.LaTeX
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.AMSMath
--import Text.LaTeX.Base.Texy

--import Data.Either

{-////////////////| Inicialización |\\\\\\\\\\\\\\\\-}
defaultFont :: PDFFont
defaultFont = PDFFont Roman Normalsize []

initTStyle :: TStyle
initTStyle = TStyle defaultFont HCenter

initTitle :: Title
initTitle = T "Nuevo Reporte" initTStyle

initPStyle :: PStyle
initPStyle = PStyle A4 defaultFont False True

initRepo :: Connection -> Repo
initRepo conn = R initTitle (C "" Style1) initPStyle conn
{-\\\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////-}


{-/////////////////| Ejecución |\\\\\\\\\\\\\\\\\-}
{-generate :: Repo -> IO()
generate repo@(R ttl cont pstl conn) = do xs <- nQuickQuery conn cont []
                                          execLaTeXT (simple repo xs) >>= renderFile (get_title repo ++ ".tex")
                                          
-}                                          
generate :: Repo -> IO()
generate repo@(R _ _ _ conn) = 
    do xs <- catchSql query (\_ -> return Nothing)
       case xs of
           Nothing  -> do putStrLn "ERROR: Consulta incorrecta o inexistente"
           Just xs' -> do execLaTeXT (simple repo xs') >>= renderFile (name ++ ".tex")
                          del <- rawSystem "pdflatex" [name++".tex"] -- "> /dev/null 2>&1"
                          del' <- rawSystem "rm" [name++".tex", name++".log", name++".aux"]
                          return ()
       where query = nQuickQuery conn (get_query repo) []
             name  = get_title repo

simple :: Monad m => Repo -> [[Maybe String]] -> LaTeXT m ()
simple repo xs = do
 thePreamble repo
 document $ theBody repo xs
 
thePreamble :: Monad m => Repo -> LaTeXT m ()
thePreamble repo = do
    documentclass [NoTitlePage] article --aca va landscape
    usepackage [Text.LaTeX.Packages.Inputenc.utf8] inputenc
    usepackage [] amsmath
    importGeometry [GPaper $ get_psize repo]
    date ""
    -- author "Paula Borrero" booleano para autor?
    -- fecha bool?
    title $ fromString $ get_title repo


theBody :: Monad m => Repo -> [[Maybe String]] -> LaTeXT m ()
theBody repo xs = do maketitle
                     theTable (map (fromString.(T.unpack)) $ head fill) m 30
                        where fill = fillMatrix xs 
                              m    = (fromLists $ tail fill)
                      
--theTable :: Monad m => [String] -> Matrix Text -> Int -> LaTeXT m ()
theTable colsname xs n = if nrows xs <= n
                         then do center $ matrixTabular colsname xs
                         else do center $ matrixTabular colsname $ submatrix 1 n 1 (ncols xs) xs
                                 theTable colsname (submatrix (n+1) (nrows xs) 1 (ncols xs) xs) n

fillMatrix :: [[Maybe String]] -> [[Text]]
fillMatrix [] = [] 
fillMatrix (xs:xss) = (map ((T.pack).conv) xs):(fillMatrix xss)

conv :: Maybe String -> String
conv x = fromMaybe "Null" x

{-\\\\\\\\\\\\\\\\\\\\\\\///////////////////////-}

{-////////////////| Modificación |\\\\\\\\\\\\\\\\-}

{----------------Título----------------}
--Cambia el contenido del título
newtitle :: String -> Repo -> Repo
newtitle newttl (R (T ttl stl) cont bstl conn) = R (T newttl stl) cont bstl conn
{-
--Cambia todo el estilo del título
title_stl :: String -> Int -> Position -> Repo -> Repo
title_stl font size pos (R (T ttl stl) cont bstl conn) = R (T ttl stl') cont bstl conn
                                                            where stl' = change_tstl font size pos stl    
{--------------------------------------}

{----------------Cuerpo----------------}
--Cambia el contenido del cuerpo
content :: String -> Repo -> Repo
content cont' (R ttl cont bstl conn) = R ttl cont' bstl conn

--Cambia fuente del cuerpo
body_font :: String -> Int -> Repo -> Repo
body_font font size (R ttl cont (BStyle psz bfont) conn) = R ttl cont (BStyle psz (PDFFont font size)) conn

--Cambia tamaño de página
pagesize :: PaperType -> Repo -> Repo
pagesize psz' (R ttl cont (BStyle psz bfont) conn) = R ttl cont (BStyle psz' bfont) conn
{--------------------------------------}
-}
{-////////////////| Auxiliares |\\\\\\\\\\\\\\\\-}
get_connection :: Repo -> Connection
get_connection (R _ _ _ conn) = conn

get_title :: Repo -> String
get_title (R (T ttl _) _ _ _) = if (length ttl') == 0 then "Reporte" else ttl'
                                    where ttl' = T.unpack $T.strip $ T.pack ttl

get_title_font :: Repo -> PDFFont
get_title_font (R (T _ (TStyle tfont _)) _ _ _) = tfont
 
get_psize :: Repo -> PaperType
get_psize (R _ _ (PStyle psz _ _ _) _) = psz

get_pttl :: Repo -> Bool
get_pttl (R _ _ (PStyle _ _ _ ttl) _) = ttl

get_plands :: Repo -> Bool
get_plands (R _ _ (PStyle _ _ ls _) _) = ls

get_pfont :: Repo -> PDFFont
get_pfont (R _ _ (PStyle _ font _ _) _) = font

get_query :: Repo -> String
get_query (R _ (C cont _) _ _) = cont

{-
change_tstl :: String -> Int -> Position -> TStyle -> TStyle
change_tstl font size pos _ = TStyle (PDFFont font size) pos
-}
