module AST where

import Database.HDBC.Sqlite3 (Connection)
import Graphics.PDF (PDFFont, FontName)
--import Data.Bool

{- data Repo = Repo Title Content BStyle Connection

donde:
* Title contiene información sobre el título del reporte
* Content es un string que contiene una query SQL
* BStyle es el estilo del cuerpo del reporte
* Connection es la información para conectarse a la base de datos
-}

type Content = String

data Repo = R Title Content BStyle Connection --Estructura del reporte

data Title = T Content TStyle

data TStyle = TStyle PDFFont Position --PDFFont = PDFFont FontName Size
	deriving Show

data BStyle = BStyle PageSize PDFFont -- agregar Margins Header FooterBool
	deriving Show

data Position = PCenter
              | PLeft
              | PRight
              | PJustified
	deriving Show

data PageSize = A4
              | Legal
              | Other Int Int --width, height
    deriving Show
