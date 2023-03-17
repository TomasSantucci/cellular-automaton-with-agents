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

getCell :: MyPoint -> Game -> Agent
getCell pos (agents, dimensions) = (V.!) agents idx
  where idx = pointToIdx pos dimensions

direction :: Int -> Int -> (Int, Int)
direction n sight = (x - sight, y - sight)
  where totalRows = 2*sight + 1
        fixPos n total | total <= n = 0
                       | div total 2 <= n = n+1
                       | otherwise = n
        useN = fixPos (abs n) (totalRows*totalRows-1)
        y = div useN totalRows
        x = useN - totalRows * y

directions :: Int -> [(Int,Int)]
directions n = [(x,y) | y <- [-n .. n], x <- [-n .. n], not ((x == 0) && (y == 0))]

getNeighbor :: Game -> Neighbor -> Agent -> Agent
getNeighbor game neigh agent = let (x,y) = agentPoint agent 
                                   (x',y') = direction (neigh-1) (agentSight agent)
                               in getCell (x+x',y+y') game

getNeighbors :: Agent -> Game -> [MyPoint] -> [Agent]
getNeighbors agent game dirs = getNeighsAux agent game dirs
  where (x,y) = agentPoint agent
        getNeighsAux _ _ [] = []
        getNeighsAux ag gam ((x',y'):dirsLeft) = (getCell (x+x',y+y') gam):(getNeighsAux ag gam dirsLeft)

countNeighs :: Eq a => (Agent -> a) -> a -> Neighbors -> Game -> Agent -> Int
countNeighs agentFunction s AllNeighbors game agent
  = foldr addCount 0 $ getNeighbors agent game $ directions (agentSight agent)
    where addCount ag i = if agentFunction ag == s then i+1 else i
countNeighs agentFunction s (Neighbors n m) game agent
  = foldr addCount 0 $ getNeighbors agent game $ slice (n-1) (m-1) $ directions (agentSight agent)
    where addCount ag i = if agentFunction ag == s then i+1 else i