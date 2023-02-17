module GameOfLifeAgents (runSims, simsToGrids) where

import AST
import Agents
import Graphics.Gloss
import ParseGame (parseFile)
import System.Random (newStdGen, StdGen)
import Data.Maybe
import Data.Vector as V (fromList, imap, map, toList, empty, null, Vector)
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')

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

drawModel :: Int -> [Model] -> Picture
drawModel cellSize [] = startScreen 0
drawModel cellSize (((game,_),n):_) = if isEmptyGame game then startScreen n else cellsToPicture game cellSize

runSims :: [StdGen -> (Game, Int)] -> Int -> Int -> IO ()
runSims sims cellSize speed =
  do rngs <- getRandomGens (length sims)
     let listOfGames = initialModel : (getRandomGames 0 rngs sims)
     simulate screen (greyN 0.3) speed listOfGames (drawModel cellSize) (\_ _ model -> nextModel model)
  where screen = InWindow "game" (900,900) (500,0)

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

-- suponemos que siempre cae dentro de los limites, que en realidad siempre pasa
idxToPoint :: Int -> MyPoint -> MyPoint
idxToPoint idx (xLim, _) = (mod idx xLim, div idx xLim)

assignPositions :: MyPoint -> V.Vector Agent -> V.Vector Agent
assignPositions dimensions agents
  = V.imap (\idx -> \agent -> setPosition agent (idxToPoint idx dimensions)) agents

simsToGrids :: [Simulation] -> IO [StdGen -> (Game,Int)]
simsToGrids [] = return []
simsToGrids ((Simulation agents dimensions iterations):rest)
  = do r2 <- simsToGrids rest
       let r1 = createGrid agents dimensions iterations
       return (r1:r2)

simsToGrids ((SimulationPath path iterations agentsDefined):rest)
  = do r2 <- simsToGrids rest
       r1 <- createGridWithPath path iterations agentsDefined
       return (r1:r2)

handleFileParsing :: String -> String -> IO ([Agent], MyPoint)
handleFileParsing path contents
  = case parseFile path contents of
      Left _ -> ioError $ userError $ "Error parsing " ++ path
      Right r -> return r

createGridWithPath :: String -> Int -> [(Agent, Int)] -> IO (StdGen -> (Game,Int))
createGridWithPath path iterations agentsDefined =
  do contents <- readFile path
     (rawCells, dimensions) <- handleFileParsing path contents
     let limitedSightAgents = fixAgentsSights agentsDefined dimensions
         cells = V.map (fillAgent limitedSightAgents)
                       $ assignPositions dimensions $ V.fromList rawCells
     return (\_ -> ((cells, dimensions), iterations))

createGrid :: [(Agent,Int)] -> MyPoint -> Int -> (StdGen -> (Game,Int))
createGrid agents (x,y) iterations
  = \rng -> let sortedAgents = multiplyAgents $ fixAgentsSights agents (x,y)
                shuffledAgents = V.fromList $ shuffle' sortedAgents (x*y) rng
                cells = assignPositions (x,y) shuffledAgents
            in ((cells, (x,y)), iterations)