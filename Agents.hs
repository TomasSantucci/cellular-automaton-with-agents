module Agents where

import AST
import qualified Data.List as L

fixAgentsSights :: [(Agent, Int)] -> MyPoint -> [(Agent, Int)]
fixAgentsSights agents (x,y) = L.map (\agent -> correctSight agent (min x y)) agents

setPosition :: Agent -> MyPoint -> Agent
setPosition (Agent name _ status states transitions sight atts) point
  = Agent name point status states transitions sight atts

copyAgent :: Agent -> Agent -> Agent
copyAgent ref (Agent name point status _ _ _ _) =
  Agent name point status (agentColors ref) (agentRules ref) (agentSight ref) (agentAttributes ref)

fillAgent :: [(Agent, Int)] -> Agent -> Agent
fillAgent agentsDefined agent = case L.find (\(ag,_) -> (agentType ag) == (agentType agent)) agentsDefined of
                                  Nothing -> agent
                                  Just (ref,_) -> copyAgent ref agent

multiplyAgents :: [(Agent, Int)] -> [Agent]
multiplyAgents [] = []
multiplyAgents ((ag,amount):rest) = (buildList ag amount) ++ (multiplyAgents rest)
  where buildList _ 0 = []
        buildList a amountLeft = a:(buildList a (amountLeft - 1))

changeSight :: Agent -> Int -> Agent
changeSight (Agent name point state colors rules _ atts) sight
  = Agent name point state colors rules sight atts

correctSight :: (Agent, Int) -> Int -> (Agent, Int)
correctSight (ag,amount) maxSight 
  | (agentSight ag) < maxSight = (ag, amount)
  | otherwise = (changeSight ag (maxSight - 1),amount)

updateList :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
updateList (s,v) [] = [(s,v)]
updateList (s,v) ((s',v'):xs) = if s == s' then (s,v):xs
                                           else (s',v'):(updateList (s,v) xs)

updateAtt :: Agent -> (String, Int) -> Agent
updateAtt (Agent n p st cols t s atts) att = Agent n p st cols t s (updateList att atts)