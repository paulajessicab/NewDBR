module AST(

    module AST
  
) where

import Database.HDBC.Sqlite3 (Connection)
import Text.LaTeX.Base.Commands(PaperType)
import Text.LaTeX.Base.Types(HPos,HPos(HCenter),HPos(HLeft),HPos(HRight))

{- data Repo = Repo Title Content BStyle Connection
donde:
* Title contiene información sobre el título del reporte
* Content es un string que contiene una query SQL
* PStyle es el estilo del cuerpo del reporte
* Connection es la información para conectarse a la base de datos
-}

data Repo = R Title Content PStyle Connection --Estructura del reporte

data Title = T String TStyle

--Query Bordes FuenteCuerpo FuenteTitulo ColumnasPrimera ColumnasResto
data Content = C String TableStyle PDFFont PDFFont Integer Integer

--ExternalV,ExternalH, InternalV, InternalH
type TableStyle = (Vert,Bool,Vert,Bool)

data Vert = None
          | Vert
          | DVert
    deriving Show

data TStyle = TStyle PDFFont HPos
    deriving Show

data PStyle = PStyle PaperType Landscape ShowTitle
    deriving Show

type Landscape = Bool
type ShowTitle = Bool

data PDFFont = PDFFont FontFamily FontSize [FontStyle]
    deriving Show

data FontFamily = Roman         --textrm = rmfamily
                | SansSerif     --textsf = sffamily
                | Mono          --texttt = ttfamily
                deriving Show

data FontSize = Tiny          --tiny
              | Scriptsize    --scriptsize
              | Footnote      --footnotesize
              | Small         --small
              | Normalsize    --normalsize
              | Large         --large
              | Large2        --Large
              | Large3        --LARGE
              | Huge          --huge
              | Huge2         --Huge
              deriving Show

data FontStyle = Normal     --textnormal 
               | Medium     --textmd
               | Bold       --textbf
               | Italic     --textit
               | SmallCaps  --textsc
               | Slanted    --textsl
               | Upright    --textup
               | Underline  --underline
               deriving Show
