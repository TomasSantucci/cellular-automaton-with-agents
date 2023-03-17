module Cellular (runSims) where

import AST
import Agents
import Graphics.Gloss
import System.Random (newStdGen, StdGen)
import Data.Vector as V (map, toList, empty, null, Vector)

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

nextModel :: [(Game, Int)] -> [(Game,Int)]
nextModel [] = []
nextModel [(game,0)] = [(game,0)]
nextModel ((game,iterations):rest)
  | iterations == 0 = rest
  | otherwise = ((nextGameState (cells,dimensions)),iterations-1):rest
    where cells = Prelude.fst game
          dimensions = Prelude.snd game

drawModel :: Int -> [(Game,Int)] -> Picture
drawModel _ [] = Blank
drawModel cellSize ((game,_):r) = cellsToPicture game cellSize

runSims :: [(Game, Int)] -> Int -> Int -> IO ()
runSims sims cellSize speed
  = simulate screen (greyN 0.6) speed sims (drawModel cellSize) (\_ _ model -> nextModel model)
  where screen = InWindow "Simulation" (900,900) (500,0)

nextGameState :: Game -> Game
nextGameState game@(cells, dimensions)
  = let newCells = V.map (\ag -> nextAgentState game (agentCurrRules ag) ag) cells
    in (newCells, dimensions)

nextAgentState :: Game -> [Rule] -> Agent -> Agent
nextAgentState _ [] agent = agent
nextAgentState game (f:fs) agent = case f game agent of
                                      Nothing -> nextAgentState game fs agent
                                      Just (NewState st) -> agentSetState agent st
                                      Just (ChangeAttribute att v) -> nextAgentState game fs (agentSetAtt agent att v)