module Eval (eval) where

import AST
import Monads
import Data.Vector as V ((!))
import Graphics.Gloss  

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

parseRule :: MonadError m => (Exp Bool) -> Result (Exp Int) -> m Rule
parseRule boolexp (NewState newState)
  = do boolFun <- expToFunction boolexp
       return $ \game agent -> if boolFun game agent
                               then Just (NewState newState)
                               else Nothing

parseRule boolexp (ChangeAttribute attName intexp)
  = do boolFun <- expToFunction boolexp
       intFun <- expToFunction intexp
       return $ \game agent -> if boolFun game agent
                               then Just (ChangeAttribute attName (intFun game agent))
                               else Nothing

rulesList :: MonadError m => RulesComm -> m [(State, Rule)]
rulesList (DefRule state boolexp res) = do rule <- parseRule boolexp res
                                           return [(state, rule)]
rulesList (Seq c1 c2) = do rules1 <- rulesList c1
                           rules2 <- rulesList c2
                           return (rules1 ++ rules2)

validateColor :: MyColor -> Either String Color
validateColor (ColorName "black") = Right black
validateColor (ColorName "white") = Right white
validateColor (ColorName "red") = Right red
validateColor (ColorName "green") = Right green
validateColor (ColorName "blue") = Right blue
validateColor (ColorName "yellow") = Right yellow
validateColor (ColorName n) = Left $ "Unknown color " ++ n
validateColor (ColorMake r g b) = Right $ makeColorI (f r) (f g) (f b) 255
  where f n = mod n 256

statesList :: (MonadState m, MonadError m) => StatesComm -> m [(State,Color)]
statesList (DefState st colName) = case validateColor colName of
                                     Left e -> throw e
                                     Right col -> return [(st,col)]
statesList (SeqSt c1 c2) = do st1 <- statesList c1
                              st2 <- statesList c2
                              return (st1 ++ st2)

attributesList :: Attributes -> [(String, Int)]
attributesList NoAtt = []
attributesList (Attribute s v) = [(s,v)]
attributesList (SeqAtt att1 att2) = (attributesList att1) ++ (attributesList att2)

pointToIdx :: MyPoint -> MyPoint -> Int
pointToIdx (x,y) (xLim, yLim) = let newX = fixPos x xLim
                                    newY = fixPos y yLim
                                in xLim * newY + newX
  where fixPos m n = if m < 0 then m + n else mod m n

getCell :: MyPoint -> Game -> Agent
getCell pos (agents, dimensions) = (V.!) agents idx
  where idx = pointToIdx pos dimensions

direction :: Int -> Int -> (Int, Int)
direction n sight = (x - sight, y - sight)
  where totalRows = 2*sight + 1
        fixPos n total | total <= n = 0
                       | div total 2 <= n = n+1
                       | otherwise = n
        useN = fixPos (abs n) (totalRows*totalRows-1)
        y = div useN totalRows
        x = useN - totalRows * y

directions :: Int -> [(Int,Int)]
directions n = [(x,y) | y <- [-n .. n], x <- [-n .. n], not ((x == 0) && (y == 0))]

findNeighbor :: Game -> Neighbor -> Agent -> Agent
findNeighbor game neigh agent = let (x,y) = agentPoint agent 
                                    (x',y') = direction (neigh-1) (agentSight agent)
                                in getCell (x+x',y+y') game

getNeighbors :: Agent -> Game -> [MyPoint] -> [Agent]
getNeighbors agent game dirs = getNeighsAux agent game dirs
  where (x,y) = agentPoint agent
        getNeighsAux _ _ [] = []
        getNeighsAux ag gam ((x',y'):dirsLeft) = (getCell (x+x',y+y') gam):(getNeighsAux ag gam dirsLeft)

findCount :: (Agent -> String) -> String -> Neighbors -> Game -> Agent -> Int
findCount fAg s AllNeighbors game agent
  = foldr f 0 $ getNeighbors agent game $ directions (agentSight agent)
    where f ag n = if fAg ag == s then n+1 else n
findCount fAg s (Neighbors n m) game agent
  = foldr f 0 $ getNeighbors agent game $ slice (n-1) (m-1) $ directions (agentSight agent)
    where f ag i = if fAg ag == s then i+1 else i

getAttValue :: String -> Agent -> Int
getAttValue attName agent = case lookup attName (agentAttributes agent) of
                              Nothing -> 0
                              Just v -> v

parseBinaryExp :: MonadError m => (Exp a) -> (Exp a) -> (a -> a -> b) -> m (Game -> Agent -> b)
parseBinaryExp e1 e2 f
  = do r1 <- expToFunction e1
       r2 <- expToFunction e2
       return (\game agent -> f (r1 game agent) (r2 game agent))

expToFunction :: MonadError m => Exp a -> m (Game -> Agent -> a)
expToFunction (Const n) = return (\_ _ -> n)
expToFunction (TypeCount name neighs) = return (findCount agentType name neighs)
expToFunction (StateCount status neighs) = return (findCount agentState status neighs)
expToFunction (Att attName) = return (\_ agent -> getAttValue attName agent)
expToFunction (Plus ie1 ie2) = parseBinaryExp ie1 ie2 (+)
expToFunction (Minus ie1 ie2) = parseBinaryExp ie1 ie2 (-)
expToFunction (Times ie1 ie2) = parseBinaryExp ie1 ie2 (*)
expToFunction (Div _ 0) = throw "Div by zero"
expToFunction (Div ie n) = do r <- expToFunction ie
                              return (\game agent -> div (r game agent) n)

expToFunction ExpFalse = return (\_ _ -> False)
expToFunction ExpTrue = return (\_ _ -> True)
expToFunction (Eq ie1 ie2) = parseBinaryExp ie1 ie2 (==)
expToFunction (Lt ie1 ie2) = parseBinaryExp ie1 ie2 (<)
expToFunction (Gt ie1 ie2) = parseBinaryExp ie1 ie2 (>)
expToFunction (And b1 b2) = parseBinaryExp b1 b2 (&&)
expToFunction (Or b1 b2) = parseBinaryExp b1 b2 (||)
expToFunction (Not b) = do be1 <- expToFunction b
                           return (\game agent -> not (be1 game agent))
expToFunction (EqState neighbor stname)
  = return (\game agent -> (agentState (findNeighbor game neighbor agent)) == stname)
expToFunction (EqAgent neighbor agname)
  = return (\game agent -> agentType (findNeighbor game neighbor agent) == agname)

checkPredicate :: (MonadState m, MonadError m) => a -> (a -> Bool) -> String -> m ()
checkPredicate a p s = if p a then throw s else return ()

checkAgentsSum :: Int -> [(Agent,Int)] -> Bool
checkAgentsSum r agents = r /= amountDefined
  where amountDefined = foldr (\(_,amount) accum -> amount + accum) 0 agents

eval :: (MonadState m, MonadError m) => Comm -> m ()
eval (DefAgent name sight atts statesComm rulesComm)
  = do states <- statesList statesComm
       rules <- rulesList rulesComm
       let attList = attributesList atts
       addAgent (Agent name (0,0) (fst (head states)) states rules sight attList)

eval (SetAgent agname n) = setAgent agname n
eval (UnsetAgent agname) = unsetAgent agname
eval (Iterations i) = setIterations i
eval (Setup n m) = do i <- getIterations
                      checkPredicate i (0 ==) "Number of iterations unset"
                      agents <- getAgents
                      checkPredicate agents null "No agents defined"
                      checkPredicate agents (checkAgentsSum (n*m)) "Agents set differ from dimensions"
                      addSimulation (Simulation agents (n,m) i)

eval (SetupPath path) = do i <- getIterations
                           checkPredicate i (0 ==) "Number of iterations unset"
                           agentsDefined <- getAgents
                           checkPredicate agentsDefined null "No agents defined"
                           addSimulation (SimulationPath path i agentsDefined)

eval (SeqComm c1 c2) = do eval c1
                          eval c2