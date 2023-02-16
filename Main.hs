module Main where

import AST
import Monads
import GameOfLifeAgents (get)
import Environment
import Eval (eval)
import Parse
import ParseGame (parseFile)
import Data.Vector as V (fromList, imap, Vector, map)
import Data.List
import System.Random (newStdGen, mkStdGen, StdGen)
import System.Random.Shuffle (shuffle')
import Graphics.Gloss

-- creating cells grid

main :: IO ()
main = do r <- readFile "./example.sim"
          case sim_parse r of
            Ok command -> exec command
            Failed e -> putStr e

exec :: Comm -> IO ()
exec c = case stateErrorGetEnv (runStateError (eval c) initEnv) of
           Left e -> print e
           Right env -> do sims <- sims2grids $ envGetSimulations env
                           get sims

setPosition :: Agent -> MyPoint -> Agent
setPosition (Agent name _ status states transitions sight atts) point
  = Agent name point status states transitions sight atts

-- suponemos que siempre cae dentro de los limites, que en realidad siempre pasa
idxToPoint :: Int -> MyPoint -> MyPoint
idxToPoint idx (xLim, _) = (mod idx xLim, div idx xLim)

assignPositions :: MyPoint -> V.Vector Agent -> V.Vector Agent
assignPositions dimensions agents
  = V.imap (\idx -> \agent -> setPosition agent (idxToPoint idx dimensions)) agents

sims2grids :: [Simulation] -> IO [StdGen -> (Game,Int)]
sims2grids [] = return []
sims2grids ((Simulation ags (x,y) it):rest) =
  do r2 <- sims2grids rest
     let r1 = createGrid ags (x,y) it
     return (r1:r2)

sims2grids ((SimulationPath path it agentsDefined):rest) =
  do r2 <- sims2grids rest
     r1 <- createGridPath path it agentsDefined
     return (r1:r2)

copyAgent :: Agent -> Agent -> Agent
copyAgent ref (Agent name point status _ _ _ _) =
  Agent name point status (agentColors ref) (agentTransitions ref) (agentSight ref) (agentAttributes ref)

fillAgent :: [(Agent, Int)] -> Agent -> Agent
fillAgent agentsDefined agent = case find (\(ag,_) -> (agentType ag) == (agentType agent)) agentsDefined of
                                  Nothing -> agent
                                  Just (ref,_) -> copyAgent ref agent

createGridPath :: String -> Int -> [(Agent, Int)] -> IO (StdGen -> (Game,Int))
createGridPath path it agentsDefined =
  do contents <- readFile path
     (rawCells, (x,y)) <- case parseFile path contents of
                            Left _ -> ioError $ userError $ "Error parsing " ++ path
                            Right r -> return r
     let vectorCells = V.fromList rawCells
         maxSight = min x y
         agentsDefinedWellSighted = Data.List.map (\ag -> correctSight ag maxSight) agentsDefined
         posCells = assignPositions (x,y) vectorCells
         filledAgents = V.map (fillAgent agentsDefinedWellSighted) posCells
     return (\_ -> ((filledAgents, (x,y)), it))

createGrid :: [(Agent,Int)] -> MyPoint -> Int -> (StdGen -> (Game,Int))
createGrid ags (x,y) it 
  = \rng -> let shuffledAgents = V.fromList $ shuffle' sortedAgents (x*y) rng
                cells = assignPositions (x,y) shuffledAgents
            in ((cells, (x,y)), it)
  where maxSight = min x y
        sortedAgents = (multiplyAgents (Data.List.map (\ag -> correctSight ag maxSight) ags))

multiplyAgents :: [(Agent, Int)] -> [Agent]
multiplyAgents [] = []
multiplyAgents ((ag,amount):rest) = (buildList ag amount) ++ (multiplyAgents rest)
  where buildList _ 0 = []
        buildList a amountLeft = a:(buildList a (amountLeft - 1))

changeSight :: Agent -> Int -> Agent
changeSight agent sight = Agent (agentType agent) (agentPoint agent)
                                (agentStatus agent) (agentColors agent) (agentTransitions agent) sight (agentAttributes agent)

correctSight :: (Agent, Int) -> Int -> (Agent, Int)
correctSight (ag,amount) maxSight 
  | (agentSight ag) < maxSight = (ag, amount)
  | otherwise = (changeSight ag (maxSight - 1),amount)
