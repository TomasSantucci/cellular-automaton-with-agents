module Eval (eval) where

import AST
import Monads
import Data.Vector as V ((!))
import Graphics.Gloss  

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

parseRule :: MonadError m => BoolExp -> UnparsedResult -> m (Game -> Agent -> Maybe Result)
parseRule boolexp (Left newState)
  = do boolFun <- boolExpToFunction boolexp
       return $ \game agent -> if boolFun game agent
                               then Just (Left newState)
                               else Nothing

parseRule boolexp (Right (attName, intexp))
  = do boolFun <- boolExpToFunction boolexp
       intFun <- intExpToFunction intexp
       return $ \game agent -> if boolFun game agent
                               then Just (Right (attName, (intFun game agent)))
                               else Nothing

rulesList :: MonadError m => RulesComm -> m [Rule]
rulesList (DefRule state boolexp res)
  = do rule <- parseRule boolexp res
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
validateColor (ColorMake r g b a) = Right $ makeColorI (f r) (f g) (f b) (f a)
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

parseBinaryIntExp :: MonadError m => IntExp -> IntExp -> (Int -> Int -> a) -> m (Game -> Agent -> a)
parseBinaryIntExp ie1 ie2 f
  = do r1 <- intExpToFunction ie1
       r2 <- intExpToFunction ie2
       return (\game agent -> f (r1 game agent) (r2 game agent))

intExpToFunction :: MonadError m => IntExp -> m (Game -> Agent -> Int)
intExpToFunction (Const n) = return (\_ _ -> n)
intExpToFunction (TypeCount name neighs) =
  return (findCount agentType name neighs)
intExpToFunction (StateCount status neighs) =
  return (findCount agentState status neighs)
intExpToFunction (Att attName) =
  return (\_ agent -> getAttValue attName agent)
intExpToFunction (Plus ie1 ie2) = parseBinaryIntExp ie1 ie2 (+)
intExpToFunction (Minus ie1 ie2) = parseBinaryIntExp ie1 ie2 (-)
intExpToFunction (Times ie1 ie2) = parseBinaryIntExp ie1 ie2 (*)
intExpToFunction (Div _ 0) = throw "Div by zero"
intExpToFunction (Div ie n) = do r <- intExpToFunction ie
                                 return (\game agent -> div (r game agent) n)

boolExpToFunction :: MonadError m => BoolExp -> m (Game -> Agent -> Bool)
boolExpToFunction ExpFalse = return (\_ _ -> False)
boolExpToFunction ExpTrue = return (\_ _ -> True)
boolExpToFunction (Eq ie1 ie2) = parseBinaryIntExp ie1 ie2 (==)
boolExpToFunction (Lt ie1 ie2) = parseBinaryIntExp ie1 ie2 (<)
boolExpToFunction (Gt ie1 ie2) = parseBinaryIntExp ie1 ie2 (>)

boolExpToFunction (And b1 b2)
  = do be1 <- boolExpToFunction b1
       be2 <- boolExpToFunction b2
       return (\game agent -> (be1 game agent) && (be2 game agent))

boolExpToFunction (Or b1 b2)
  = do be1 <- boolExpToFunction b1
       be2 <- boolExpToFunction b2
       return (\game agent -> (be1 game agent) || (be2 game agent))

boolExpToFunction (Not b)
  = do be1 <- boolExpToFunction b
       return (\game agent -> not (be1 game agent))

boolExpToFunction (EqState neighbor stname)
  = return (\game agent -> (agentState (findNeighbor game neighbor agent)) == stname)

boolExpToFunction (EqAgent neighbor agname)
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
       checkPredicate states null "No states defined"
       checkPredicate rules null ("No transition rules defined for " ++ name)
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

eval (SetupPath path) = do agentsDefined <- getAgents
                           checkPredicate agentsDefined null "No agents defined"
                           i <- getIterations
                           checkPredicate i (0 ==) "Number of iterations unset"
                           addSimulation (SimulationPath path i agentsDefined)

eval (SeqComm c1 c2) = do eval c1
                          eval c2

