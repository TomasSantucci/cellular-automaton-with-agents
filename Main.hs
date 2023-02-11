module Main where

import AST
import Monads
import GameOfLifeAgents (get)
import Environment
import Eval (eval)
import Parse
import ParseGame (parseFile)
import TestCommands
import Data.Vector as V (fromList, imap, Vector)
import System.Random (newStdGen, mkStdGen, StdGen)
import System.Random.Shuffle (shuffle')
import Graphics.Gloss

-- creating cells grid

main :: IO ()
main = do r <- readFile "./example.sim"
          let (Ok command3) = (sim_parse r)
          print command3
          case stateErrorGetEnv (runStateError (eval command3) initEnv) of
            Left error -> print error
            Right env -> do let sims = map Main.simulate $ envGetSimulations env
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

simulate :: Simulation -> (StdGen -> (Game,Int))
simulate (Simulation ags (x,y) it file)
  = \rng -> let shuffledAgents = V.fromList $ shuffle' sortedAgents (x*y) rng
                cells = assignPositions (x,y) shuffledAgents
            in ((cells, (x,y)), it)
  where maxSight = min x y
        sortedAgents = (multiplyAgents (map (\ag -> correctSight ag maxSight) ags))

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
