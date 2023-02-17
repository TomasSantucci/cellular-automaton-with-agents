module Main (main) where

import AST
import Agents
import Monads
import GameOfLifeAgents (runSims)
import Environment
import Eval (eval)
import Parse
import ParseGame (parseFile)
import Data.Vector as V (fromList, imap, Vector, map)
import qualified Data.List as L
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')

main :: IO ()
main = do r <- readFile "./cyclic_automaton.sim"
          case sim_parse r of
            Ok command -> execComm command
            Failed e -> putStr e

execComm :: Comm -> IO ()
execComm c = case stateErrorGetEnv (runStateError (eval c) initEnv) of
               Left e -> print e
               Right env -> do sims <- simsToGrids $ envGetSimulations env
                               runSims sims

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