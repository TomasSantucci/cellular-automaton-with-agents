module Main where

import AST
import Monads
import GameOfLifeAgents (get)
import Environment
import Eval (eval)
import TestCommands
import Data.Vector as V (fromList, imap, Vector)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

-- creating cells grid

main :: IO ()
main = case stateErrorGetEnv (runStateError (eval command) initEnv) of
            Left _ -> return ()
            Right env -> sequence_ $ map simulate $ envGetSimulations env

setPosition :: Agent -> Point -> Agent
setPosition (Agent name _ status transitions sight) point
  = Agent name point status transitions sight

-- suponemos que siempre cae dentro de los limites, que en realidad siempre pasa
idxToPoint :: Int -> Point -> Point
idxToPoint idx (xLim, _) = (mod idx xLim, div idx xLim)

assignPositions :: Point -> V.Vector Agent -> V.Vector Agent
assignPositions dimensions agents
  = V.imap (\idx -> \agent -> setPosition agent (idxToPoint idx dimensions)) agents

simulate :: Simulation -> IO ()
simulate (Simulation ags (x,y) it file) 
  = do rng <- newStdGen
       let sortedAgents = (multiplyAgents (map (\ag -> correctSight ag maxSight) ags))
       let shuffledAgents = V.fromList $ shuffle' sortedAgents (x*y) rng
       let cells = assignPositions (x,y) shuffledAgents
       writeFile file ""
       get (cells, (x,y)) it file
  where maxSight = min x y

multiplyAgents :: [(Agent, Int)] -> [Agent]
multiplyAgents [] = []
multiplyAgents ((ag,amount):rest) = (buildList ag amount) ++ (multiplyAgents rest)
  where buildList _ 0 = []
        buildList a amountLeft = a:(buildList a (amountLeft - 1))

changeSight :: Agent -> Int -> Agent
changeSight agent sight = Agent (agentType agent) (agentPoint agent)
                                (agentStatus agent) (agentTransitions agent) sight

correctSight :: (Agent, Int) -> Int -> (Agent, Int)
correctSight (ag,amount) maxSight 
  | (agentSight ag) < maxSight = (ag, amount)
  | otherwise = (changeSight ag (maxSight - 1),amount)