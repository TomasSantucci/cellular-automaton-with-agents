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

getRandomGens :: Int -> IO ([StdGen])
getRandomGens 1 = do rng <- newStdGen
                     return [rng]
getRandomGens n = do rng <- newStdGen
                     rngs <- getRandomGens (n-1)
                     return $ (rng:rngs)

getRandomModels :: Int -> [StdGen] -> [StdGen -> (Game,Int)] -> [Model]
getRandomModels _ [] [] = []
getRandomModels simIndex (x:xs) (f:fs)
  = (f x, simIndex):(getRandomModels (simIndex+1) xs fs)
getRandomModels _ _ _ = []

emptyGame :: Game
emptyGame = (V.empty, (0,0))

isEmptyGame :: Game -> Bool
isEmptyGame (cells,_) = V.null cells

startScreen :: Int -> Picture
startScreen n = pictures [pic2,pic1]
  where pic1 = color white $ Translate (-125) (0) $ Scale 0.3 0.3 $ Text ("Simulation " ++ (show n))
        pic2 = Translate (-10) 10 $ color black $ rectangleSolid 300 100

initialModel :: Model
initialModel = ((emptyGame, 0), 0)

nextModel :: [Model] -> [Model]
nextModel [] = []
nextModel [((game,0),_)] = [((game,0),0)]
nextModel (((game,iterations),index):rest)
  | V.null cells = rest
  | iterations == 0 = ((emptyGame,0),index+1):rest
  | otherwise = (((nextGameState (cells,dimensions)),iterations-1),index):rest
    where cells = Prelude.fst game
          dimensions = Prelude.snd game

drawModel :: Int -> [Model] -> Picture
drawModel _ [] = startScreen 0
drawModel cellSize (((game,_),n):_) = if isEmptyGame game then startScreen n else cellsToPicture game cellSize

runSims :: [StdGen -> (Game, Int)] -> Int -> Int -> IO ()
runSims sims cellSize speed =
  do rngs <- getRandomGens (length sims)
     let listOfGames = initialModel : (getRandomModels 0 rngs sims)
     simulate screen (greyN 0.3) speed listOfGames (drawModel cellSize) (\_ _ model -> nextModel model)
  where screen = InWindow "game" (900,900) (500,0)

nextGameState :: Game -> Game
nextGameState game@(cells, dimensions)
  = let newCells = V.map (\ag -> nextAgentState game (filterRules ag) ag) cells
    in (newCells, dimensions)

nextAgentState :: Game -> [Game -> Agent -> Maybe Result] -> Agent -> Agent
nextAgentState _ [] agent = agent
nextAgentState game (f:fs) agent = case f game agent of
                                      Nothing -> nextAgentState game fs agent
                                      Just (Left st) -> updateState agent st
                                      Just (Right att) -> nextAgentState game fs (updateAtt agent att)