module EvalSim (evalSim) where

import AST
import Cellular (runSims)
import Data.Vector as V (fromList, imap, map, Vector)
import Agents
import ParseGrid (parseFile)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

assignPositions :: MyPoint -> V.Vector Agent -> V.Vector Agent
assignPositions dimensions agents
  = V.imap (\idx -> \agent -> agentSetPos agent (idxToPoint idx dimensions)) agents
    where idxToPoint idx (xLim, _) = (mod idx xLim, div idx xLim)

handleFileParsing :: String -> String -> IO ([Agent], MyPoint)
handleFileParsing path contents
  = case parseFile path contents of
      Left _ -> ioError $ userError $ "Error parsing " ++ path
      Right (cells,dims) ->
        if null cells then ioError $ userError $ "Las dimensiones no coinciden en: " ++ path
                      else return (cells,dims)

createGrid :: Simulation -> IO (Grid,Int)
createGrid (Simulation agents (x,y) iterations)
  = do rng <- newStdGen
       let sortedAgents = agentsExpand $ agentsFixSight agents (x,y)
           shuffledAgents = V.fromList $ shuffle' sortedAgents (x*y) rng
           cells = assignPositions (x,y) shuffledAgents
       return ((cells, (x,y)), iterations)

createGrid (SimulationPath path iterations agentsDefined)
  = do contents <- readFile path
       (rawCells, dimensions) <- handleFileParsing path contents
       let limitedSightAgents = agentsFixSight agentsDefined dimensions
           cells = V.map (agentFill limitedSightAgents)
                         $ assignPositions dimensions $ V.fromList rawCells
       return ((cells, dimensions), iterations)


simsToGrids :: [Simulation] -> IO [(Grid,Int)]
simsToGrids [] = return []
simsToGrids (sim:rest)
  = do r2 <- simsToGrids rest
       r1 <- createGrid sim
       return (r1:r2)

evalSim :: Int -> Int -> [Simulation] -> IO ()
evalSim cellSize fps sims
  = do grids <- simsToGrids sims
       runSims grids cellSize fps