module GameOfLifeAgents (runSims) where

import AST
import Agents
import Graphics.Gloss
import System.Random (newStdGen, StdGen)
import Data.Maybe
import Data.Vector as V (map, toList, empty, null, Vector)

cellToPicture :: Agent -> Picture
cellToPicture (Agent _ (x,y) state colors _ _ _)
  = translate (fromIntegral (x*cellSize)) (fromIntegral (-y*cellSize)) $ color col rectangle
  where col = case lookup state colors of
                Nothing -> greyN 0.5
                Just c -> c
        rectangle = rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
        cellSize = 30

cellsToPicture :: (V.Vector Agent, b) -> Picture
cellsToPicture (cells,_) = pictures $ V.toList $ V.map cellToPicture cells

getRandomGens :: Int -> IO ([StdGen])
getRandomGens 1 = do rng <- newStdGen
                     return [rng]
getRandomGens n = do rng <- newStdGen
                     rngs <- getRandomGens (n-1)
                     return $ (rng:rngs)

getRandomGames :: Int -> [StdGen] -> [StdGen -> (Game,Int)] -> [Model]
getRandomGames _ [] [] = []
getRandomGames simIndex (x:xs) (f:fs)
  = (f x, simIndex):(getRandomGames (simIndex+1) xs fs)

emptyGame :: Game
emptyGame = (V.empty, (0,0))

isEmptyGame :: Game -> Bool
isEmptyGame (cells,_) = V.null cells

startScreen :: Int -> Picture
startScreen n = pictures [pic2,pic1]
  where pic1 = color white $ Translate (-125) (0) $ Scale 0.3 0.3 $ Text ("Simulacion " ++ (show n))
        pic2 = Translate (-10) 10 $ color black $ rectangleSolid 300 100

initialModel :: Model
initialModel = ((emptyGame, 0), 0)

nextModel :: [Model] -> [Model]
nextModel [((game,0),_)] = [((game,0),0)]
nextModel (((game,iterations),index):rest)
  | V.null cells = rest
  | iterations == 0 = ((emptyGame,0),index+1):rest
  | otherwise = (((nextState (cells,dimensions)),iterations-1),index):rest
    where cells = Prelude.fst game
          dimensions = Prelude.snd game

drawModel :: [Model] -> Picture
drawModel [] = startScreen 0
drawModel (((game,_),n):_) = if isEmptyGame game then startScreen n else cellsToPicture game

runSims :: [StdGen -> (Game, Int)] -> IO ()
runSims sims =
  do rngs <- getRandomGens (length sims)
     let listOfGames = initialModel : (getRandomGames 0 rngs sims)
     simulate screen white 10 listOfGames drawModel (\_ _ model -> nextModel model)
  where screen = InWindow "game" (500,500) (500,500)

nextState :: Game -> Game
nextState game = (V.map (`makeCell` game) (fst game),snd game)

makeCell :: Agent -> Game -> Agent
makeCell ag@(Agent name point _ colors rules sight atts) game =
  Agent name point (nextAgentState game (filterRules ag) ag) colors rules sight atts 

filterRules :: Agent -> [Game -> Agent -> Maybe Result]
filterRules agent = [y | (x,y) <- (agentRules agent), x == (agentState agent)]

nextAgentState :: Game -> [Game -> Agent -> Maybe Result] -> Agent -> State
nextAgentState _ [] agent = agentState agent
nextAgentState game (f:fs) agent = case f game agent of
                                      Nothing -> nextAgentState game fs agent
                                      Just (Left st) -> st
                                      Just (Right att) -> nextAgentState game fs (updateAtt agent att)
