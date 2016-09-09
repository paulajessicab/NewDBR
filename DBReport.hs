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
hacer parser --v
arreglar tamaños segun fuentes (hacer funcion que evalue el numero)
arreglar margenes
ver posicion dentro de tabla
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
{-
connect :: String -> Repo -> Repo
connect database (R a b c _) = let conn = connectSqlite3 database
                               in R a b c conn
-}                           


{-/////////////////| Ejecución |\\\\\\\\\\\\\\\\\-}                                      
generate :: Repo -> IO ()
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
    importGeometry [GPaper $ get_paper_size repo]
    date ""
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
                                      hfont = format $ get_table_hfont repo
                                      tfont = get_table_bfont repo
                                      tstyle = get_layout repo
--arreglar ultimo salto

fillMatrix :: [[Maybe String]] -> [[String]]
fillMatrix [] = []
fillMatrix (xs:xss) = (map conv xs):(fillMatrix xss)

conv :: Maybe String -> String
conv x = fromMaybe "Null" x
{-\\\\\\\\\\\\\\\\\\\\\\\///////////////////////-}

{-////////////////| Modificación |\\\\\\\\\\\\\\\\-}
{----------------Título----------------}
--Cambia el contenido del título
titleNew :: String -> Repo -> Repo
titleNew newttl (R (T ttl stl) cont bstl conn) = R (T newttl stl) cont bstl conn

titlePos :: HPos -> Repo -> Repo
titlePos pos (R (T ttl stl) cont bstl conn) = R (T ttl stl') cont bstl conn
                                                where stl' = chTitlePos pos stl
                                                
chTitlePos :: HPos -> TStyle -> TStyle
chTitlePos pos' (TStyle font pos) = TStyle font pos'

titleFont :: FontFamily -> Integer -> Repo -> Repo
titleFont ff n (R (T ttl stl) cont bstl conn) = R (T ttl stl') cont bstl conn
                                                where stl' = chTitleFont ff n stl

chTitleFont :: FontFamily -> Integer -> TStyle -> TStyle
chTitleFont ff' n (TStyle (PDFFont ff fs stls) p) =
                               TStyle (PDFFont ff' (intToSize n) stls) p

titleDecor :: [FontStyle] -> Repo -> Repo
titleDecor ds (R (T ttl stl) cont bstl conn) = R (T ttl stl') cont bstl conn
                                                where stl' = chTitleDecor ds stl
                                                
chTitleDecor :: [FontStyle] -> TStyle -> TStyle
chTitleDecor styles (TStyle (PDFFont ff fs sts) p) = TStyle (PDFFont ff fs styles) p 
{--------------------------------------}

{----------------Cuerpo----------------}
--Cambia el contenido del cuerpo
query :: String -> Repo -> Repo--ver
query q' (R ttl (C q ts nf tf) bstl conn) = R ttl (C q' ts nf tf) bstl conn

paperSize :: PaperType -> Repo -> Repo
paperSize sz (R ttl cont pstl conn) = R ttl cont (chPaperSize sz pstl) conn
                                        
chPaperSize :: PaperType -> PStyle -> PStyle
chPaperSize sz' (PStyle sz land title) = PStyle sz' land title

paperLands :: Bool -> Repo -> Repo
paperLands b (R ttl cont pstl conn) = R ttl cont (chPaperLands b pstl) conn
                                        
chPaperLands :: Bool -> PStyle -> PStyle
chPaperLands b (PStyle sz land title) = PStyle sz b title

paperTitle :: Bool -> Repo -> Repo
paperTitle b (R ttl cont pstl conn) = R ttl cont (chPaperTitle b pstl) conn
                                        
chPaperTitle :: Bool -> PStyle -> PStyle
chPaperTitle b (PStyle sz land title) = PStyle sz land b

tableLayout :: Vert -> Bool -> Vert -> Bool -> Repo -> Repo
tableLayout b0 b1 b2 b3 (R ttl cont pstl conn) =
                               R ttl (chTableLO b0 b1 b2 b3 cont) pstl conn 
                                                   
chTableLO :: Vert -> Bool -> Vert -> Bool -> Content -> Content
chTableLO b0 b1 b2 b3 (C s ts f0 f1) = C s (b0,b1,b2,b3) f0 f1

tableBFont :: FontFamily -> Integer -> Repo -> Repo
tableBFont ff n (R ttl cont pstl conn) =
                               R ttl (chTableBFont ff n cont) pstl conn
      
chTableBFont :: FontFamily -> Integer -> Content -> Content
chTableBFont ff n (C str ts (PDFFont ff0 fs0 ds0) f1) = 
                              C str ts (PDFFont ff (intToSize n) ds0) f1

tableHFont :: FontFamily -> Integer -> Repo -> Repo
tableHFont ff n (R ttl cont pstl conn) =
                               R ttl (chTableHFont ff n cont) pstl conn
                               
chTableHFont :: FontFamily -> Integer -> Content -> Content
chTableHFont ff n (C str ts f0 (PDFFont ff1 fs1 ds1)) = 
                              C str ts f0 (PDFFont ff (intToSize n) ds1)
    
tableBDecor :: [FontStyle] -> Repo -> Repo
tableBDecor ds (R ttl cont pstl conn) = R ttl (chTableBDec ds cont) pstl conn

chTableBDec :: [FontStyle] -> Content -> Content
chTableBDec ds (C str ts (PDFFont ff0 fs0 ds0) f1) = 
                                        C str ts (PDFFont ff0 fs0 ds) f1

tableHDecor :: [FontStyle] -> Repo -> Repo
tableHDecor ds (R ttl cont pstl conn) = R ttl (chTableHDec ds cont) pstl conn

chTableHDec :: [FontStyle] -> Content -> Content
chTableHDec ds (C str ts f0 (PDFFont ff1 fs1 ds1)) = 
                                        C str ts f0 (PDFFont ff1 fs1 ds)
{--------------------------------------}      

{-////////////////| Auxiliares |\\\\\\\\\\\\\\\\-}
get_connection :: Repo -> Connection
get_connection (R _ _ _ conn) = conn

get_title :: Repo -> String
get_title (R (T ttl _) _ _ _) = if (length ttl') == 0 then "Reporte" else ttl'
                                    where ttl' = T.unpack $T.strip $ T.pack ttl

get_title_font :: Repo -> PDFFont
get_title_font (R (T _ (TStyle tfont _)) _ _ _) = tfont

get_title_pos :: Repo -> HPos
get_title_pos (R (T _ (TStyle _ tpos)) _ _ _) = tpos

get_query :: Repo -> String
get_query (R _ (C cont _ _ _) _ _) = cont

get_layout :: Repo -> (Vert,Bool,Vert,Bool)
get_layout (R _ (C _ tstyle _ _) _ _) = tstyle

get_table_hfont :: Repo -> PDFFont
get_table_hfont (R _ (C _ _ hfont _) _ _) = hfont

get_table_bfont :: Repo -> PDFFont
get_table_bfont (R _ (C _ _ _ bfont) _ _) = bfont

get_paper_size :: Repo -> PaperType
get_paper_size (R _ _ (PStyle psz _ _) _) = psz

get_paper_lands :: Repo -> Landscape
get_paper_lands (R _ _ (PStyle _ lands _) _) = lands

get_paper_title :: Repo -> ShowTitle
get_paper_title (R _ _ (PStyle _ _ showtitle) _) = showtitle

-------

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

to_fsize Tiny       = tiny
to_fsize Scriptsize = scriptsize
to_fsize Footnote   = footnotesize
to_fsize Small      = small
to_fsize Normalsize = normalsize
to_fsize Large      = large
to_fsize Large2     = large2
to_fsize Large3     = large3
to_fsize Huge       = huge
to_fsize Huge2      = huge2

to_ffam Roman     = textrm
to_ffam SansSerif = textsf
to_ffam Mono      = texttt 

to_fstyles []     = textnormal
to_fstyles [x]    = to_fsty x
to_fstyles (x:xs) = (to_fsty x).(to_fstyles xs)
                   
to_fsty Normal    = textnormal
to_fsty Medium    = textmd
to_fsty Bold      = textbf
to_fsty Italic    = textit
to_fsty SmallCaps = textsc
to_fsty Slanted   = textsl
to_fsty Upright   = textup
to_fsty Underline = underline

intToSize :: Integer -> FontSize
intToSize 2 = Scriptsize
intToSize 3 = Footnote
intToSize 4 = Small
intToSize 5 = Normalsize
intToSize 6 = Large
intToSize 7 = Large2
intToSize 8 = Large3
intToSize 9 = Huge
intToSize n | n < 2 = Tiny
            | n > 9 = Huge2
