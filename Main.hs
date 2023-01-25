module Main where

import AST
import Monads
import Environment
--import Data.Strict.Tuple as T
import Data.Vector as V ((!), fromList)
--import Data.Maybe
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

command :: Comm
command = SeqComm (DefAgent "ag1" 3 (Transition 0 ExpTrue 1))
         (SeqComm (DefAgent "ag2" 1 (Transition 1 ExpFalse 0))
         (SeqComm (SetAgent "ag1" 6)
         (SeqComm (SetAgent "ag2" 3)
         (SeqComm (Iterations 3)
         (Setup 3 3 "file")))))

main :: IO ()
main = case stateErrorGetEnv (runStateError (eval command) initEnv) of
            Left _ -> return ()
            Right env -> sequence_ $ map simulate (envGetSimulations env)

simulate :: Simulation -> IO ()
simulate (Simulation ags (x,y) _ file) 
  = do rng <- newStdGen
       print $ V.fromList $ shuffle' (createGame (map (\ag -> correctSight ag maxSight) ags)) (x*y) rng
  where maxSight = min x y

createGame :: [(Agent, Int)] -> [Agent]
createGame [] = []
createGame ((ag,amount):rest) = (buildList ag amount) ++ (createGame rest)
  where buildList _ 0 = []
        buildList a amountLeft = a:(buildList a (amountLeft - 1))

changeSight :: Agent -> Int -> Agent
changeSight agent sight = Agent (agentType agent) (agentPoint agent)
                                (agentStatus agent) (agentTransitions agent) sight

correctSight :: (Agent, Int) -> Int -> (Agent, Int)
correctSight (ag,amount) maxSight 
  | (agentSight ag) < maxSight = (ag, amount)
  | otherwise = (changeSight ag (maxSight - 1),amount)

-- Slices a list
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Parsing functions

-- Takes a data for a transition and returns a transition function
parseTransition :: BoolExp -> Status -> (Game -> Agent -> Maybe Status)
parseTransition boolexp state = \game -> \agent ->
  if (transformBoolExp boolexp) game agent then Just state else Nothing

-- Takes a TransitionComm datatype and returns a list of the transitions
transitionsList :: TransitionComm -> Transitions
transitionsList (Transition iniSt boolexp endSt) = [(iniSt, parseTransition boolexp endSt)]
transitionsList (Seq t1 t2) = (transitionsList t1) ++ (transitionsList t2)

-- Returns the agent placed in the position given in the game
getCell :: Point -> Game -> Agent
getCell pos (agents, (xLim, yLim)) = let (newX,newY) = (fixPos (Prelude.fst pos) xLim, fixPos (Prelude.snd pos) yLim)
                                     in (V.!) agents (xLim * newX + newY)
  where fixPos x m = if x < 0 then m + x else mod x m

-- Given a sight and a certain neighbor, returns its position
direction :: Int -> Int -> (Int, Int)
direction n sight = (row - sight, col - sight)
  where totalRows = 2*sight + 1
        useN = if n > div (totalRows^2-1) 2 then n+1 else n
        row = div useN totalRows
        col = useN - totalRows * row

-- Given the sight of an agent returns all the directions towards its neighbors
directions :: Int -> [(Int,Int)]
directions n = [(x,y) | y <- [-n .. n], x <- [-n .. n], not ((x == 0) && (y == 0))]

-- use mod already used in getCell
-- Finds a neighbor given an agent, number and game
findNeighbor :: Game -> Neighbor -> Agent -> Agent
findNeighbor game (Neighbor n) agent = let (x,y) = agentPoint agent 
                                           (x',y') = direction n (agentSight agent)
                                       in getCell (x+x',y+y') game

-- Given an agent and a game returns a list of its neighbors
getNeighbors :: Agent -> Game -> [Point] -> [Agent]
getNeighbors agent game dirs = getNeighsAux agent game dirs
  where (x,y) = agentPoint agent
        getNeighsAux _ _ [] = []
        getNeighsAux ag gam ((x',y'):dirsLeft) = (getCell (x+x',y+y') gam):(getNeighsAux ag gam dirsLeft)

-- Given an agent returns the number of neighbors that satisfy the count condition
findCount :: Counts -> Agent -> Game -> Int
findCount (TypeCount agname AllNeighbors) agent game = foldr f 0 $ getNeighbors agent game $ directions (agentSight agent)
  where f ag n = if (agentType ag) == agname then n+1 else n
findCount (TypeCount agname (Neighbors n m)) agent game = foldr f 0 $ getNeighbors agent game $ slice n m $ directions (agentSight agent)
  where f ag i = if (agentType ag) == agname then i+1 else i
findCount (StateCount agname AllNeighbors) agent game = foldr f 0 $ getNeighbors agent game $ directions (agentSight agent)
  where f ag n = if (agentStatus ag) == agname then n+1 else n
findCount (StateCount agname (Neighbors n m)) agent game = foldr f 0 $ getNeighbors agent game $ slice n m $ directions (agentSight agent)
  where f ag i = if (agentStatus ag) == agname then i+1 else i

-- Transforms a bool expression into a function that receives a game
-- and an agent and returns True if the expression is satisfied
transformBoolExp :: BoolExp -> (Game -> Agent -> Bool)
transformBoolExp (And b1 b2) = \game -> \agent -> ((transformBoolExp b1) game agent) && ((transformBoolExp b2) game agent)
transformBoolExp (Or b1 b2) = \game -> \agent -> ((transformBoolExp b1) game agent) || ((transformBoolExp b2) game agent)
transformBoolExp (Not b) = \game -> \agent -> not $ (transformBoolExp b) game agent
transformBoolExp (EqState neighbor stname) = \game -> \agent -> agentStatus (findNeighbor game neighbor agent) == stname
transformBoolExp (EqAgent neighbor agname) = \game -> \agent -> agentType (findNeighbor game neighbor agent) == agname
transformBoolExp (EqCount counts n) = \game -> \agent -> (findCount counts agent game) == n
transformBoolExp ExpFalse= \_ -> \_ -> False 
transformBoolExp ExpTrue = \_ -> \_ -> True

checkPositiveIterations :: (MonadState m, MonadError m) => Int -> m ()
checkPositiveIterations i = if i == 0 then throw "Number of iterations unset"
                                      else return ()

checkEmptyList :: (MonadState m, MonadError m) => [a] -> m ()
checkEmptyList xs = if null xs then throw "No agents defined"
                               else return ()

eval :: (MonadState m, MonadError m) => Comm -> m ()
eval (DefAgent name sight states) = do let t = transitionsList states
                                       _ <- if null t then throw "No transitions defined for an agent"
                                                      else addAgent (Agent name (0,0) (Prelude.fst (head t)) t sight)
                                       return ()
eval (SetAgent agname n) = setAgent agname n
--eval (RemoveAgent agname) = removeAgent agname
eval (Iterations i) = setIterations i
eval (Setup n m file) = do i <- getIterations
                           checkPositiveIterations i
                           agents <- getAgents
                           checkEmptyList agents
                           addSimulation (Simulation agents (n,m) i file)
                           return ()
--eval (SeqComm Skip c) = eval c
eval (SeqComm c1 c2) = do eval c1
                          eval c2