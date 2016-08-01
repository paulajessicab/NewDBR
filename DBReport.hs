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
--PDF
import Graphics.PDF
import Graphics.PDF.Typesetting
--
import Control.Monad.State
import Data.Bool
import Control.Exception
import Data.Maybe
--import Data.Either

{-////////////////| Inicialización |\\\\\\\\\\\\\\\\-}
defaultFont :: PDFFont
defaultFont = PDFFont Times_Roman 10

initTStyle :: TStyle
initTStyle = TStyle defaultFont PCenter

initTitle :: Title
initTitle = T "Nuevo Reporte" initTStyle

initBStyle :: BStyle
initBStyle = BStyle A4_H defaultFont

initRepo :: Connection -> Repo
initRepo conn = R initTitle "" initBStyle conn
{-\\\\\\\\\\\\\\\\\\\\\\\\\/////////////////////////-}


{-/////////////////| Ejecución |\\\\\\\\\\\\\\\\\-}

-- Función generadora:
-- Realiza la consulta, arregla los datos y "genera el pdf"
generate :: Repo -> IO()
generate repo@(R ttl cont bstl conn) = do xs <- nQuickQuery conn cont [] --ver error de consulta
                                     
                                          let pdfFileName = (get_title repo) ++ ".pdf"
                                          let documentInfo = standardDocInfo 
                                          let pageSize = toPageSize $ get_psize repo
                                          let pdfFont = get_body_font repo
                                          let titleFont = get_title_font repo  
                                          let content = reArrange $ padAll $ fillMatrix $ xs
                                         
                                          printMatrix $ padAll $ fillMatrix $ xs
                                        
                                          runPdf pdfFileName documentInfo pageSize $ do
                                           page <- addPage Nothing
                                           drawWithPage page $ do 
                                              drawText $ do
                                                 setFont pdfFont
                                                 wordSpace $ getDescent pdfFont
                                                 textStart 2 (297.0 - 2* (getDescent pdfFont))
                                                 leading $ getHeight pdfFont
                                                 displayText $ toPDFString " "
--gen content

gen []  = do displayText $ toPDFString " "
gen xss = do displayText $ toPDFString $ head xss
             startNewLine
             gen $ tail xss
            
-- Funciones de relleno
padAll :: [([String],Int)] -> [[String]]
padAll xss = map (\(xs,n) -> map (pad n) xs) xss

--https://gist.github.com/hanshoglund/5941143
pad :: Int -> String -> String
pad n s | length s < n = s ++ replicate (n - length s) ' '
        | otherwise    = s

--Función de debug -- Transpone nuevamente la matriz y la imprime
printMatrix :: [[String]] -> IO ()
printMatrix [] = putStr ""
printMatrix ([]:xs) = putStr ""
printMatrix xss = do putStrLn $ unwords h
                     printMatrix $ map tail xss
                      where h = map head xss

reArrange :: [[String]] -> [String]
reArrange ([]:xss) = []
reArrange xss = ((unwords h):(reArrange $ map tail $ xss))
                        where h = map head xss


--Se traspone la matriz y se guarda el ancho de la cadena máxima de cada columna                            
fillMatrix :: [[Maybe String]] -> [([String],Int)]
fillMatrix [] = []
fillMatrix ([]:xs) = []
fillMatrix xss = case map tail xss of
                    [] -> [runState (convRow' (map head xss)) 0]
                    _  ->  (runState (convRow' (map head xss)) 0) : (fillMatrix (map tail xss))
 
convRow' :: [Maybe String] -> State Int [String]
convRow' []     = return []
convRow' [x]    = do curr <- get
                     let y = conv x
                     if (length y > curr) then do {put (length y); return [y]} else return [y]
convRow' (x:xs) = do curr <- get
                     let y = conv x
                     if (length y > curr) then do {put (length y); ys <- (convRow' xs); return (y:ys)} else do {ys <- (convRow' xs); return (y:ys)}

conv :: Maybe String -> String
conv x = fromMaybe "Null" x
{-\\\\\\\\\\\\\\\\\\\\\\\///////////////////////-}

{-////////////////| Modificación |\\\\\\\\\\\\\\\\-}

{----------------Título----------------}
--Cambia el contenido del título
title :: String -> Repo -> Repo
title newttl (R (T ttl stl) cont bstl conn) = R (T newttl stl) cont bstl conn

--Cambia todo el estilo del título
title_stl :: FontName -> Int -> Position -> Repo -> Repo
title_stl font size pos (R (T ttl stl) cont bstl conn) = R (T ttl stl') cont bstl conn
                                                            where stl' = change_tstl font size pos stl    
{--------------------------------------}

{----------------Cuerpo----------------}
--Cambia el contenido del cuerpo
content :: String -> Repo -> Repo
content cont' (R ttl cont bstl conn) = R ttl cont' bstl conn

--Cambia fuente del cuerpo
body_font :: FontName -> Int -> Repo -> Repo
body_font font size (R ttl cont (BStyle psz bfont) conn) = R ttl cont (BStyle psz (PDFFont font size)) conn

--Cambia tamaño de página
body_page_size :: PageSize -> Repo -> Repo
body_page_size psz' (R ttl cont (BStyle psz bfont) conn) = R ttl cont (BStyle psz' bfont) conn
{--------------------------------------}

{-////////////////| Auxiliares |\\\\\\\\\\\\\\\\-}
get_connection :: Repo -> Connection
get_connection (R _ _ _ conn) = conn

get_title :: Repo -> String
get_title (R (T ttl _) _ _ _) = ttl

get_title_font :: Repo -> PDFFont
get_title_font (R (T _ (TStyle tfont _)) _ _ _) = tfont
 
get_psize :: Repo -> PageSize
get_psize (R _ _ (BStyle psz _) _) = psz

toPageSize :: PageSize -> PDFRect
toPageSize A4_H    = PDFRect 0 0 596 841 --10 = 4 mm --100 = 35mm
toPageSize Legal_H   = PDFRect 0 0 216 356 --acomodar
toPageSize (Other x y) = PDFRect 0 0 x y

get_body_font :: Repo -> PDFFont
get_body_font (R _ _ (BStyle _ bfont) _) = bfont

change_tstl :: FontName -> Int -> Position -> TStyle -> TStyle
change_tstl font size pos _ = TStyle (PDFFont font size) pos

