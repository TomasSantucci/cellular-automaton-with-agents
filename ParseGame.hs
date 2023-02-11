module ParseGame where

import AST
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

lis :: TokenParser u
lis = makeTokenParser (emptyDef)

{-
-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    {reservedNames   = ["true", "false", "if", "else", "while", "skip" , "do"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "?"
                        , ":"
                        ]
    }
  )
-}

escapeP p = try p <|> (do char '\n'
                          escapeP p)
                  <|> (do char ' '
                          escapeP p)

parseCell = do name <- many1 alphaNum 
               char '-'
               status <- many1 alphaNum 
               return (Agent name (0,0) status [] [] 0 [])

--parseGame :: Int -> Parser Game
parseGame = sepBy (escapeP parseCell) (escapeP $ char ',')

getGame :: Parser ([Agent], MyPoint)
getGame = do x <- natural lis
             char ','
             y <- natural lis
             game <- brackets lis parseGame
             if (length game) == ((fromIntegral x) *(fromIntegral y)) then return (game,(fromIntegral x, fromIntegral y))
                                   else return ([],(0,0))

parseFile = parse (totParser getGame)

{-
main = do s <- readFile "./game.txt"
          print $ parseFile "game.txt" s
-}