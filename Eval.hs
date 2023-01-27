module Eval where

import AST
import Monads
import Data.Vector as V ((!))

-- Parsing functions

-- Slices a list
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Takes a data for a transition and returns a transition function
parseTransition :: BoolExp -> Status -> (Game -> Agent -> Maybe Status)
parseTransition boolexp state = \game -> \agent ->
  if (transformBoolExp boolexp) game agent then Just state else Nothing

-- Takes a TransitionComm datatype and returns a list of the transitions
transitionsList :: TransitionComm -> Transitions
transitionsList (Transition iniSt boolexp endSt) = [(iniSt, parseTransition boolexp endSt)]
transitionsList (Seq t1 t2) = (transitionsList t1) ++ (transitionsList t2)

-- Transforms a 2-d point into an index of a 1-d vector
pointToIdx :: Point -> Point -> Int
pointToIdx (x,y) (xLim, yLim) = let newX = fixPos x xLim
                                    newY = fixPos y yLim
                                in xLim * newY + newX
  where fixPos m n = if m < 0 then m + n else mod m n

-- Returns the agent placed in the position given in the game
getCell :: Point -> Game -> Agent
getCell pos (agents, dimensions) = (V.!) agents idx
  where idx = pointToIdx pos dimensions

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
findCount (TypeCount agname (Neighbors n m)) agent game = foldr f 0 $ getNeighbors agent game $ slice (n-1) (m-1) $ directions (agentSight agent)
  where f ag i = if (agentType ag) == agname then i+1 else i
findCount (StateCount agname AllNeighbors) agent game = foldr f 0 $ getNeighbors agent game $ directions (agentSight agent)
  where f ag n = if (agentStatus ag) == agname then n+1 else n
findCount (StateCount agname (Neighbors n m)) agent game = foldr f 0 $ getNeighbors agent game $ slice (n-1) (m-1) $ directions (agentSight agent)
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