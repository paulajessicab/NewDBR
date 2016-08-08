module AST where

import Database.HDBC.Sqlite3 (Connection)
import Data.Bool
import Text.LaTeX.Base.Commands(PaperType)
{- data Repo = Repo Title Content BStyle Connection

donde:
* Title contiene información sobre el título del reporte
* Content es un string que contiene una query SQL
* BStyle es el estilo del cuerpo del reporte
* Connection es la información para conectarse a la base de datos
-}

type Content = String
type Landscape = Bool

data Repo = R Title Content BStyle Connection --Estructura del reporte

data Title = T Content TStyle

data TStyle = TStyle PDFFont Position --PDFFont = PDFFont FontName Size
        deriving Show

data PDFFont = PDFFont String Int
    deriving Show

data BStyle = BStyle PaperType PDFFont -- agregar Margins Header FooterBool Landscape
    deriving Show

data Position = PCenter
              | PLeft
              | PRight
              | PJustified
        deriving Show

{-PaperType 

A0	 
A1	 
A2	 
A3	 
A4	 
A5	 
A6	 
B0	 
B1	 
B2	 
B3	 
B4	 
B5	 
B6	 
Letter	 
Executive	 
Legal
-}
