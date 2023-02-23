module Main (main) where

import AST
import Agents
import Monads
import Cellular (runSims)
import Data.Vector as V (fromList, imap, map, Vector)
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')
import ParseGrid (parseFile)
import Environment
import Eval (eval)
import Parse
import Text.Read (readMaybe)
import System.Environment

main :: IO ()
main = do args <- getArgs
          (s,(cellSize, speed)) <- parseArgs args
          r <- readFile s
          case sim_parse r of
            Ok command -> execComm command cellSize speed
            Failed e -> putStr e

parseArgs :: [String] -> IO (String,(Int,Int))
parseArgs args = case args of
                   [s] -> return (s,(20,3))
                   [s,cellSize] -> do r <- readArg cellSize
                                      return (s,(r,3))
                   [s,cellSize,speed] -> do r1 <- readArg cellSize
                                            r2 <- readArg speed
                                            return (s,(r1,r2))
                   _ -> ioError $ userError "Error parsing arguments"

readArg :: String -> IO Int
readArg arg = case readMaybe arg of
                Just n -> return n
                Nothing -> ioError $ userError "Error parsing argument"

execComm :: Comm -> Int -> Int -> IO ()
execComm c cellSize speed
  = case stateErrorGetEnv (runStateError (eval c) initEnv) of
      Left e -> print e
      Right env -> do sims <- simsToGrids $ envGetSimulations env
                      runSims sims cellSize speed

assignPositions :: MyPoint -> V.Vector Agent -> V.Vector Agent
assignPositions dimensions agents
  = V.imap (\idx -> \agent -> setPosition agent (idxToPoint idx dimensions)) agents
    where idxToPoint idx (xLim, _) = (mod idx xLim, div idx xLim)

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