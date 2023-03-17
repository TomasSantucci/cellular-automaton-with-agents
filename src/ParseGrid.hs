module ParseGrid (parseFile) where

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
lis = makeTokenParser (emptyDef {reservedOpNames  = ["-", ","]})

parseCell :: Parser Agent
parseCell = do name <- identifier lis
               reservedOp lis "-"
               state <- identifier lis
               return (Agent name (0,0) state [] [] 0 [])

parseGrid :: Parser [Agent]
parseGrid = sepBy parseCell (reservedOp lis ",")

getGrid :: Parser ([Agent], MyPoint)
getGrid = do x <- natural lis
             reservedOp lis ","
             y <- natural lis
             grid <- brackets lis parseGrid
             if (length grid) == ((fromIntegral x) * (fromIntegral y))
               then return (grid,(fromIntegral x, fromIntegral y))
               else return ([],(0,0))

parseFile :: SourceName -> String -> Either ParseError ([Agent], MyPoint)
parseFile = parse (totParser getGrid)
