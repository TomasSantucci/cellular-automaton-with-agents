module Cellular (runSims) where

import AST
import Agents
import Graphics.Gloss
import Data.Vector as V (map, toList, Vector)

myTranslate :: Int -> Int -> Picture -> Picture
myTranslate x y p = translate (fromIntegral x) (fromIntegral y) p

cellToPicture :: Int -> Agent -> Picture
cellToPicture cellSize (Agent _ (x,y) state colors _ _ _)
  = myTranslate (x*cellSize + (div cellSize 2)) (-y*cellSize - (div cellSize 2)) $ color col rectangle
  where col = case lookup state colors of
                Nothing -> greyN 0.5
                Just c -> c
        rectangle = rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)

cellsToPicture :: (V.Vector Agent, MyPoint) -> Int -> Picture
cellsToPicture (cells,(xDim,yDim)) cellSize = myTranslate xMove yMove grid
  where xMove = -xDim* (div cellSize 2)
        yMove = yDim* (div cellSize 2)
        grid = pictures (V.toList $ V.map (cellToPicture cellSize) cells)

nextModel :: [(Grid, Int)] -> [(Grid,Int)]
nextModel [] = []
nextModel [(grid,0)] = [(grid,0)]
nextModel ((grid,iterations):rest)
  | iterations == 0 = rest
  | otherwise = ((nextGridState (cells,dimensions)),iterations-1):rest
    where cells = fst grid
          dimensions = snd grid

drawModel :: Int -> [(Grid,Int)] -> Picture
drawModel _ [] = Blank
drawModel cellSize ((grid,_):_) = cellsToPicture grid cellSize

nextGridState :: Grid -> Grid
nextGridState grid@(cells, dimensions)
  = let newCells = V.map (\ag -> nextAgentState grid (agentCurrRules ag) ag) cells
    in (newCells, dimensions)

nextAgentState :: Grid -> [Rule] -> Agent -> Agent
nextAgentState _ [] agent = agent
nextAgentState grid (f:fs) agent = case f grid agent of
                                      Nothing -> nextAgentState grid fs agent
                                      Just (NewState st) -> agentSetState agent st
                                      Just (ChangeAttribute att v) -> nextAgentState grid fs (agentSetAtt agent att v)

runSims :: [(Grid, Int)] -> Int -> Int -> IO ()
runSims sims cellSize fps
  = simulate screen (greyN 0.6) fps sims (drawModel cellSize) (\_ _ model -> nextModel model)
  where screen = InWindow "Simulation" (900,900) (500,0)