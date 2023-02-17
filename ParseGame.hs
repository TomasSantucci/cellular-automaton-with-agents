module ParseGame (parseFile) where

import AST
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language ( emptyDef )

totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

lis :: TokenParser u
lis = makeTokenParser (emptyDef)

escapeP :: Parser a -> Parser a
escapeP p = try p <|> (do char '\n'
                          escapeP p)
                  <|> (do char ' '
                          escapeP p)

parseCell :: Parser Agent
parseCell = do name <- many1 alphaNum 
               char '-'
               status <- many1 alphaNum 
               return (Agent name (0,0) status [] [] 0 [])

parseGame :: Parser [Agent]
parseGame = sepBy (escapeP parseCell) (escapeP $ char ',')

getGame :: Parser ([Agent], MyPoint)
getGame = do x <- natural lis
             char ','
             y <- natural lis
             game <- brackets lis parseGame
             if (length game) == ((fromIntegral x) *(fromIntegral y))
               then return (game,(fromIntegral x, fromIntegral y))
               else return ([],(0,0))

parseFile :: SourceName -> String -> Either ParseError ([Agent], MyPoint)
parseFile = parse (totParser getGame)