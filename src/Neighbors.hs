module Neighbors where

import AST
import Data.Vector as V ((!))

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

pointToIdx :: MyPoint -> MyPoint -> Int
pointToIdx (x,y) (xLim, yLim) = let newX = fixPos x xLim
                                    newY = fixPos y yLim
                                in xLim * newY + newX
  where fixPos m n = if m < 0 then m + n else mod m n

getCell :: MyPoint -> Grid -> Agent
getCell pos (agents, dimensions) = (V.!) agents idx
  where idx = pointToIdx pos dimensions

direction :: Int -> Int -> (Int, Int)
direction n sight = (x - sight, y - sight)
  where totalRows = 2*sight + 1
        fixPos m total | total <= m = 0
                       | div total 2 <= m = m+1
                       | otherwise = m
        useN = fixPos (abs n) (totalRows*totalRows-1)
        y = div useN totalRows
        x = useN - totalRows * y

directions :: Int -> [(Int,Int)]
directions n = [(x,y) | y <- [-n .. n], x <- [-n .. n], not ((x == 0) && (y == 0))]

getNeighbor :: Grid -> Neighbor -> Agent -> Agent
getNeighbor grid neigh agent = let (x,y) = agentPoint agent 
                                   (x',y') = direction (neigh-1) (agentSight agent)
                               in getCell (x+x',y+y') grid

getNeighbors :: Agent -> Grid -> [MyPoint] -> [Agent]
getNeighbors agent grid dirs = getNeighsAux agent grid dirs
  where (x,y) = agentPoint agent
        getNeighsAux _ _ [] = []
        getNeighsAux ag gam ((x',y'):dirsLeft) = (getCell (x+x',y+y') gam):(getNeighsAux ag gam dirsLeft)

countNeighs :: Eq a => (Agent -> a) -> a -> Neighbors -> Grid -> Agent -> Int
countNeighs agentFunction s AllNeighbors grid agent
  = foldr addCount 0 $ getNeighbors agent grid $ directions (agentSight agent)
    where addCount ag i = if agentFunction ag == s then i+1 else i
countNeighs agentFunction s (Neighbors n m) grid agent
  = foldr addCount 0 $ getNeighbors agent grid $ slice (n-1) (m-1) $ directions (agentSight agent)
    where addCount ag i = if agentFunction ag == s then i+1 else i