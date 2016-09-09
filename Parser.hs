module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.LaTeX.Base.Types(HPos, HPos(HLeft),HPos(HRight),HPos(HCenter))
import Text.LaTeX.Base.Commands
import DBReport
import AST

totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace untyped
                  t <- p
                  eof
                  return t

untyped :: TokenParser u
untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_',
                                          reservedNames = ["title",
                                             "style","font","decor","pos",
                                             "query","select","table",
                                             "header","body","paper",
                                             "landscape","set","unset", "layout"],
                                          caseSensitive = False})

{--Parser de Comandos--}
parseCommands :: Parser [Repo->Repo]
parseCommands = many parseCmd'

parseCmd' = do x <- parseCmd
               optional $ many $ string "\n"
               return x

parseCmd :: Parser (Repo->Repo)
parseCmd = try (do reserved untyped "title"
                   x <- parseTitle
                   return x)
       <|> try (do reserved untyped "query"
                   string "select "
                   x <- many1 $ choice [(identifier untyped), (string "* ")]--parseQuoted
                   return $ query $ "select "++(unwords x))
       <|> try (do reserved untyped "table"
                   x <- parseTable
                   return x)
       <|> try (do reserved untyped "paper"
                   x <- parsePaper
                   return x)

{--Parser de Titulo--}
parseTitle :: Parser (Repo->Repo)
parseTitle = try (do reserved untyped "pos"
                     spaces
                     p <- parsePos
                     spaces
                     return $ titlePos p)
         <|> try (do parseFont "title")
         <|> try (do {x <- many1 (identifier untyped); return $ titleNew $ unwords x})--(do {x <- parseQuoted; return $ titleNew $ unwords x})

{--Parser de Estilo de Papel--}
parsePaper :: Parser (Repo -> Repo)
parsePaper = try (do reserved untyped "set"
                     spaces
                     f <- parseLabel
                     spaces
                     return $ f True)
         <|> try (do reserved untyped "unset"
                     spaces
                     f <- parseLabel
                     spaces
                     return $ f False)
         <|> try (do {spaces;x <- parseSize;spaces; return $ paperSize x})

parseSize :: Parser PaperType
parseSize = try (do {string "a0"; return A0})
        <|> try (do {string "a1"; return A1})
        <|> try (do {string "a2"; return A2})
        <|> try (do {string "a3"; return A3})
        <|> try (do {string "a4"; return A4})
        <|> try (do {string "a5"; return A5})
        <|> try (do {string "a6"; return A6})
        <|> try (do {string "b0"; return B0})
        <|> try (do {string "b1"; return B1})
        <|> try (do {string "b2"; return B2})
        <|> try (do {string "b3"; return B3})
        <|> try (do {string "b4"; return B4})
        <|> try (do {string "b5"; return B5})
        <|> try (do {string "b6"; return B6})
        <|> try (do {string "letter"; return Letter})
        <|> try (do {string "executive"; return Executive})
        <|> try (do {string "legal"; return Legal})

parseLabel :: Parser (Bool -> Repo -> Repo)
parseLabel = try (do {reserved untyped "landscape"; return paperLands})
         <|> try (do {reserved untyped "title"; return paperTitle})

{--Parser de Estilos de Tablas--}
parseTable :: Parser (Repo -> Repo)
parseTable = try (do reserved untyped "header"
                     f <- parseFont "header"
                     return f)
         <|> try (do reserved untyped "body"
                     f <- parseFont "body"
                     return f)
         <|> try (do reserved untyped "layout"
                     spaces
                     a0 <- parseVert
                     spaces
                     a1 <- parseBool
                     spaces
                     a2 <- parseVert
                     spaces
                     a3 <- parseBool
                     spaces
                     return $ tableLayout a0 a1 a2 a3)

{--Parser de Fuentes--}
parseFont :: String -> Parser (Repo->Repo)
parseFont w = try (do reserved untyped "font"
                      x <- parseFontFamily
                      n <- natural untyped
                      case w of
                        "title" -> return $ titleFont x n
                        "header" -> return $ tableHFont x n
                        "body" -> return $ tableBFont x n)
          <|> try (do reserved untyped "decor"
                      ds <- many1 parseDecor
                      case w of
                        "title" -> return $ titleDecor ds
                        "header" -> return $ tableHDecor ds
                        "body" -> return $ tableBDecor ds)

parsePos :: Parser HPos
parsePos = try (do {string "center"; return HCenter})
       <|> try (do {string "left"; return HLeft})
       <|> try (do {string "right"; return HRight})

parseFontFamily :: Parser FontFamily
parseFontFamily = try (do {string "roman";spaces; return Roman})
              <|> try (do {string "serif ";spaces; return SansSerif})
              <|> try (do {string "mono ";spaces; return Mono})

parseDecor :: Parser FontStyle
parseDecor = try (do {string "normal"; return Normal})
         <|> try (do {string "bold"; return Bold})
         <|> try (do {string "italics"; return Italic})
         <|> try (do {string "smallcaps"; return SmallCaps})
         <|> try (do {string "slanted"; return Slanted})
         <|> try (do {string "upright"; return Upright})
         <|> try (do {string "underline"; return Underline})
         
parseDecor' :: Parser FontStyle
parseDecor' = do spaces
                 x <- parseDecor
                 spaces
                 return x

{--Auxiliares--}
parseBool :: Parser Bool
parseBool = try (do {string "true"; return True})
        <|> try (do {string "false"; return False})

parseVert :: Parser Vert
parseVert = try (do {string "none"; return None})
        <|> try (do {string "single"; return Vert})
        <|> try (do {string "double"; return DVert})
