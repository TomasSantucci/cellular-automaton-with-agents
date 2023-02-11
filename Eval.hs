module Eval where

import AST
import Monads
import Data.Vector as V ((!))
import Graphics.Gloss  

-- Parsing functions

-- Slices a list
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Takes a data for a transition and returns a transition function
parseTransition :: BoolExp -> Result -> (Game -> Agent -> Maybe Result)
parseTransition boolexp res = \game -> \agent ->
  if (transformBoolExp boolexp) game agent then Just res else Nothing

attributesList :: Attributes -> [(String, Int)]
attributesList (Attribute s v) = [(s,v)]
attributesList NoAtt = []
attributesList (SeqAtt att1 att2) = (attributesList att1) ++ (attributesList att2)

-- Takes a TransitionComm datatype and returns a list of the transitions
transitionsList :: TransitionComm -> Transitions
transitionsList (Transition iniSt boolexp res) = [(iniSt, parseTransition boolexp res)]
transitionsList (Seq t1 t2) = (transitionsList t1) ++ (transitionsList t2)

-- Transforms a 2-d point into an index of a 1-d vector
pointToIdx :: MyPoint -> MyPoint -> Int
pointToIdx (x,y) (xLim, yLim) = let newX = fixPos x xLim
                                    newY = fixPos y yLim
                                in xLim * newY + newX
  where fixPos m n = if m < 0 then m + n else mod m n

-- Returns the agent placed in the position given in the game
getCell :: MyPoint -> Game -> Agent
getCell pos (agents, dimensions) = (V.!) agents idx
  where idx = pointToIdx pos dimensions

-- Given a sight and a certain neighbor, returns its position
direction :: Int -> Int -> (Int, Int)
direction n sight = (x - sight, y - sight)
  where totalRows = 2*sight + 1
        useN = if n >= div (totalRows^2-1) 2 then n+1 else n
        y = div useN totalRows
        x = useN - totalRows * y

-- Given the sight of an agent returns all the directions towards its neighbors
directions :: Int -> [(Int,Int)]
directions n = [(x,y) | y <- [-n .. n], x <- [-n .. n], not ((x == 0) && (y == 0))]

-- Finds a neighbor given an agent, number and game
findNeighbor :: Game -> Neighbor -> Agent -> Agent
findNeighbor game (Neighbor n) agent = let (x,y) = agentPoint agent 
                                           (x',y') = direction (n-1) (agentSight agent)
                                       in getCell (x+x',y+y') game

-- Given an agent and a game returns a list of its neighbors
getNeighbors :: Agent -> Game -> [MyPoint] -> [Agent]
getNeighbors agent game dirs = getNeighsAux agent game dirs
  where (x,y) = agentPoint agent
        getNeighsAux _ _ [] = []
        getNeighsAux ag gam ((x',y'):dirsLeft) = (getCell (x+x',y+y') gam):(getNeighsAux ag gam dirsLeft)

-- Given an agent returns the number of neighbors that satisfy the count condition
findCount :: Counts -> Agent -> Game -> Int
findCount (TypeCount agname AllNeighbors) agent game
  = foldr f 0 $ getNeighbors agent game $ directions (agentSight agent)
    where f ag n = if (agentType ag) == agname then n+1 else n
findCount (TypeCount agname (Neighbors n m)) agent game
  = foldr f 0 $ getNeighbors agent game $ slice (n-1) (m-1) $ directions (agentSight agent)
    where f ag i = if (agentType ag) == agname then i+1 else i
findCount (StateCount agname AllNeighbors) agent game
  = foldr f 0 $ getNeighbors agent game $ directions (agentSight agent)
    where f ag n = if (agentStatus ag) == agname then n+1 else n
findCount (StateCount agname (Neighbors n m)) agent game
  = foldr f 0 $ getNeighbors agent game $ slice (n-1) (m-1) $ directions (agentSight agent)
    where f ag i = if (agentStatus ag) == agname then i+1 else i

compareAtt :: (Int -> Bool) -> String -> [(String, Int)] -> Bool
compareAtt f att atts = case lookup att atts of
                          Nothing -> False
                          Just v -> f v

-- Transforms a bool expression into a function that receives a game
-- and an agent and returns True if the expression is satisfied
transformBoolExp :: BoolExp -> (Game -> Agent -> Bool)
transformBoolExp (And b1 b2) = \game agent ->
  ((transformBoolExp b1) game agent) && ((transformBoolExp b2) game agent)

transformBoolExp (Or b1 b2) = \game agent ->
  ((transformBoolExp b1) game agent) || ((transformBoolExp b2) game agent)

transformBoolExp (Not b) = \game agent ->
  not $ (transformBoolExp b) game agent

transformBoolExp (EqState neighbor stname) = \game agent ->
  (agentStatus (findNeighbor game neighbor agent)) == stname

transformBoolExp (EqAgent neighbor agname) = \game agent ->
  agentType (findNeighbor game neighbor agent) == agname

transformBoolExp (EqCount counts n) = \game agent ->
  (findCount counts agent game) == n

transformBoolExp (EqAtt att v) = \game agent ->
  compareAtt (\v' -> v == v') att (agentAttributes agent)

transformBoolExp (LtAtt att v) = \game agent ->
  compareAtt (\v' -> v' < v) att (agentAttributes agent)

transformBoolExp (GtAtt att v) = \game agent ->
  compareAtt (\v' -> v' > v) att (agentAttributes agent)

transformBoolExp ExpFalse= \_ _ -> False 
transformBoolExp ExpTrue = \_ _ -> True

checkPositiveIterations :: (MonadState m, MonadError m) => Int -> m ()
checkPositiveIterations i = if i == 0 then throw "Number of iterations unset"
                                      else return ()

checkEmptyList :: (MonadState m, MonadError m) => [a] -> String -> m ()
checkEmptyList xs s = if null xs then throw s
                                 else return ()

-- When everything works try this
checkPredicate :: (MonadState m, MonadError m) => a -> (a -> Bool) -> String -> m ()
checkPredicate a p s = if p a then throw s else return ()

validateColor :: MyColor -> Either String Color
validateColor (ColorName "black") = Right black
validateColor (ColorName "white") = Right white
validateColor (ColorName "red") = Right red
validateColor (ColorName "green") = Right green
validateColor (ColorName "blue") = Right blue
validateColor (ColorName "yellow") = Right yellow
validateColor (ColorName n) = Left n
validateColor (ColorMake r g b a) = Right $ makeColorI (f r) (f g) (f b) (f a)
  where f n = mod n 256

statesList :: (MonadState m, MonadError m) => States -> m [(Status,Color)]
statesList (State st col) = case validateColor col of
                              Left colName -> throw $ "Unknown color " ++ colName
                              Right color -> return [(st,color)]
statesList (SeqSt c1 c2) = do st1 <- statesList c1
                              st2 <- statesList c2
                              return (st1 ++ st2)

eval :: (MonadState m, MonadError m) => Comm -> m ()
eval (DefAgent name sight atts states rules) = 
  do stList <- statesList states
     let t = transitionsList rules
         attList = attributesList atts
     checkEmptyList stList "No states defined"
     checkEmptyList t "No transitions defined for an agent"
     addAgent (Agent name (0,0) (Prelude.fst (head stList)) stList t sight attList)
     return ()

eval (SetAgent agname n) = setAgent agname n
eval (UnsetAgent agname) = unsetAgent agname
eval (Iterations i) = setIterations i
eval (Setup n m) = do i <- getIterations
                      checkPositiveIterations i
                      agents <- getAgents
                      checkEmptyList agents "No agents defined"
                      addSimulation (Simulation agents (n,m) i)
                      return ()
eval (SeqComm c1 c2) = do eval c1
                          eval c2

eval (SetupPath path) = do agentsDefined <- getAgents
                           i <- getIterations
                           addSimulation (SimulationPath path i agentsDefined)
                           return ()


-- parsePath :: Path -> Either Error ([(AgName, Status)], Dimensions)
--parsePath :: String -> Either Error ([(String,Status)], MyPoint)