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

{- TODO
hacer parser
arreglar tamaños segun fuentes (hacer funcion que evalue el numero)
arreglar margenes
doble linea horizontal??
-}

{-////////////////| Inicialización |\\\\\\\\\\\\\\\\-}
defaultFont :: PDFFont
defaultFont = PDFFont Mono Normalsize [Underline]

initTStyle :: TStyle
initTStyle = TStyle defaultFont HCenter

initTitle :: Title
initTitle = T "Nuevo Reporte" initTStyle

initPStyle :: PStyle
initPStyle = PStyle A4 False True

initTableStyle :: TableStyle
initTableStyle = (Vert,True,DVert,True)

initRepo :: Connection -> Repo
initRepo conn = R initTitle (C "" initTableStyle defaultFont defaultFont) initPStyle conn
{-\\\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////-}


{-/////////////////| Ejecución |\\\\\\\\\\\\\\\\\-}                                      
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
    if lands_bool repo 
    then do documentclass [NoTitlePage, Landscape] article --arreglar filas
    else do documentclass [NoTitlePage] article
    usepackage [Text.LaTeX.Packages.Inputenc.utf8] inputenc
    usepackage [] amsmath
    importGeometry [GPaper $ get_psize repo]
    date ""
    -- author "Paula Borrero" booleano para autor?
    -- fecha bool?
    --title $ get_Tfsize repo $ fromString $ get_title repo
    title $ fstyle $ fromString $ get_title repo
        where fstyle = format $ get_title_font $ repo

theBody :: Monad m => Repo -> [[Maybe String]] -> LaTeXT m ()
theBody repo xs | title_bool repo = do maketitle
                                       print_table
                | otherwise = do print_table
                where --print_table = theTable (map (fromString.(T.unpack)) $ head fill) m 30
                      print_table = theTable colnames m repo
                      colnames    = map fromString $ head fill
                      fill        = fillMatrix xs 
                      m           = (fromLists $ tail fill)
                      
theTable colnames xs repo = if nrows xs <= n
                            then do center $ matrixTabular2 f_cnames xs tfont tstyle --acomodar
                         --then do center $ matrixTabular (map underline colsname) xs --acomodar
                            else do center $ matrixTabular2 f_cnames (submatrix 1 n 1 (ncols xs) xs) tfont tstyle
                                    theTable colnames (submatrix (n+1) (nrows xs) 1 (ncols xs) xs) repo
                                where n = get_nrows repo
                                      f_cnames = map hfont colnames
                                      hfont = format $ get_colt_font repo
                                      tfont = get_table_font repo
                                      tstyle = get_table_style repo

fillMatrix :: [[Maybe String]] -> [[String]]
fillMatrix [] = []
fillMatrix (xs:xss) = (map conv xs):(fillMatrix xss)

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

{----------------Cuerpo----------------}-}
--Cambia el contenido del cuerpo
content :: String -> Repo -> Repo
content query' (R ttl (C query ts nf tf) bstl conn) = R ttl (C query' ts nf tf) bstl conn
{-
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

get_colt_font :: Repo -> PDFFont
get_colt_font (R _ (C _ _ font _) _ _) = font

get_table_font :: Repo -> PDFFont
get_table_font (R _ (C _ _ _ font) _ _) = font
 
get_psize :: Repo -> PaperType
get_psize (R _ _ (PStyle psz _ _) _) = psz

get_pttl :: Repo -> Bool
get_pttl (R _ _ (PStyle _ _ ttl) _) = ttl

get_plands :: Repo -> Bool
get_plands (R _ _ (PStyle _ ls _) _) = ls

get_pfont :: Repo -> PDFFont
get_pfont (R _ (C _ _ font _) _ _) = font

get_query :: Repo -> String
get_query (R _ (C cont _ _ _) _ _) = cont

get_table_style :: Repo -> TableStyle
get_table_style (R _ (C _ ts _ _) _ _) = ts

get_nrows :: Repo -> Int  --Cambiar de acuerdo al tamaño!
get_nrows repo = 30

format (PDFFont ff fs xs) = (to_ffam ff).(to_fsize fs).(to_fstyles xs)
    
matrixTabular2 ts m tfont stl = --probar con intersperse 
    let spec = vspec (ncols m) stl
    in case stl of
           (_,True,_,True) ->
             tabular Nothing spec $ mconcat
             [ hline
             , foldl1 (&) ts
             , lnbk
             , hline
             , mconcat $ fmap (
               \i -> mconcat [ foldl1 (&) $ fmap (\j -> ((format tfont).fromString) (m ! (i,j))) [1 .. ncols m]
                     , lnbk
                     , hline
                     ] ) [1 .. nrows m]
             ]
           (_,True,_,False) ->
              tabular Nothing spec $ mconcat
             [ hline
             , foldl1 (&) ts
             , lnbk
             , mconcat $ (fmap (
               \i -> mconcat [ foldl1 (&) $ fmap (\j -> ((format tfont).fromString) (m ! (i,j))) [1 .. ncols m]
                     , lnbk
                     ] ) [1 .. nrows m])
             , hline
             ] 
           (_,False,_,True) ->
             tabular Nothing spec $ mconcat $
             [ foldl1 (&) ts
             , lnbk
             , hline
             , mconcat $ fmap (
               \i -> mconcat [ foldl1 (&) $ fmap (\j -> ((format tfont).fromString) (m ! (i,j))) [1 .. ncols m]
                     , lnbk
                     , hline
                     ] ) [1 .. (nrows m)-1]
             ] ++ [foldl1 (&) $ fmap (\j -> ((format tfont).fromString) (m ! (nrows m,j))) [1 .. ncols m], lnbk]
           (_,False,_,False) -> 
             tabular Nothing spec $ mconcat
             [ foldl1 (&) ts
             , lnbk
             , mconcat $ fmap (
               \i -> mconcat [ foldl1 (&) $ fmap (\j -> ((format tfont).fromString) (m ! (i,j))) [1 .. ncols m]
                     , lnbk
                     ] ) [1 .. nrows m]
             ]
             
vspec :: Int -> TableStyle -> [TableSpec]
vspec nc (None, _, None, _) = replicate nc CenterColumn
vspec nc (None, _, vert, _) =
  case vert of
    Vert  -> intersperse VerticalLine $ replicate nc CenterColumn
    DVert -> intersperse DVerticalLine $ replicate nc CenterColumn
vspec nc (vert, _, None, _) =
  case vert of
    Vert  -> VerticalLine : (replicate nc CenterColumn) ++ [VerticalLine]
    DVert -> DVerticalLine : (replicate nc CenterColumn) ++ [DVerticalLine]
vspec nc (vert1, _, vert2, _) =
  case vert1 of
    Vert  -> case vert2 of
               Vert  -> VerticalLine : (intersperse VerticalLine $ replicate nc CenterColumn) ++ [VerticalLine]
               DVert -> VerticalLine : (intersperse DVerticalLine $ replicate nc CenterColumn) ++ [VerticalLine]
    DVert -> case vert2 of
               Vert  -> DVerticalLine : (intersperse VerticalLine $ replicate nc CenterColumn) ++ [DVerticalLine]
               DVert -> DVerticalLine : (intersperse DVerticalLine $ replicate nc CenterColumn) ++ [DVerticalLine]



title_bool :: Repo -> Bool
title_bool (R _ _ (PStyle _ _ st) _)  = st

lands_bool :: Repo -> Bool
lands_bool (R _ _ (PStyle _  land _) _)  = land

to_fsize size = case size of
                    Tiny       -> tiny
                    Scriptsize -> scriptsize
                    Footnote   -> footnotesize
                    Small      -> small
                    Normalsize -> normalsize
                    Large      -> large
                    Large2     -> large2
                    Large3     -> large3
                    Huge       -> huge
                    Huge2      -> huge2

to_ffam family = case family of
                   Roman     -> textrm
                   SansSerif -> textsf
                   Mono      -> texttt

to_fstyles []     = textnormal
to_fstyles [x]      = to_fsty x
to_fstyles (x:xs) = (to_fsty x).(to_fstyles xs)
                   
to_fsty style = case style of
                   Normal    -> textnormal
                   Medium    -> textmd
                   Bold      -> textbf
                   Italic    -> textit
                   SmallCaps -> textsc
                   Slanted   -> textsl
                   Upright   -> textup
                   Underline -> underline
