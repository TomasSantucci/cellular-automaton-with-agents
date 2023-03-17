module Agents where

import AST
import qualified Data.List as L

agentSetSight :: Agent -> Int -> Agent
agentSetSight agent sight = agent {agentSight = sight}

agentSetAtt :: Agent -> String -> Int -> Agent
agentSetAtt agent att v = agent {agentAttributes = updateList (att,v) agentAtts}
  where agentAtts = agentAttributes agent

agentSetState :: Agent -> State -> Agent
agentSetState agent st = agent {agentState = st}

agentSetPos :: Agent -> MyPoint -> Agent
agentSetPos agent point = agent {agentPoint = point}

agentGetAttribute :: String -> Agent -> Int
agentGetAttribute attName agent = case lookup attName (agentAttributes agent) of
                              Nothing -> 0
                              Just v -> v

agentsFixSight :: [(Agent, Int)] -> MyPoint -> [(Agent, Int)]
agentsFixSight agents (x,y) = L.map (\agent -> agentFixSight agent (min x y)) agents

agentFixSight :: (Agent, Int) -> Int -> (Agent, Int)
agentFixSight (ag,amount) maxSight 
  | (agentSight ag) < maxSight = (ag, amount)
  | otherwise = (agentSetSight ag (maxSight - 1),amount)

agentCopy :: Agent -> Agent -> Agent
agentCopy ref (Agent name point status _ _ _ _) =
  Agent name point status (agentColors ref) (agentRules ref) (agentSight ref) (agentAttributes ref)

agentFill :: [(Agent, Int)] -> Agent -> Agent
agentFill agentsDefined agent = case L.find (\(ag,_) -> (agentType ag) == (agentType agent)) agentsDefined of
                                  Nothing -> agent
                                  Just (ref,_) -> agentCopy ref agent

agentsExpand :: [(Agent, Int)] -> [Agent]
agentsExpand [] = []
agentsExpand ((ag,amount):rest) = (buildList ag amount) ++ (agentsExpand rest)
  where buildList _ 0 = []
        buildList a amountLeft = a:(buildList a (amountLeft - 1))

updateList :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
updateList (s,v) [] = [(s,v)]
updateList (s,v) ((s',v'):xs) = if s == s' then (s,v):xs
                                           else (s',v'):(updateList (s,v) xs)

agentCurrRules :: Agent -> [Rule]
agentCurrRules agent = [y | (x,y) <- (agentRules agent), x == (agentState agent)]