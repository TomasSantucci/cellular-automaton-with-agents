module GameOfLifeAgents where

import AST
--import           Control.Concurrent
import           Data.Maybe
import Data.Vector as V (sequence_, map, imap)

representation :: Agent -> String
representation agent = "[" ++ (agentType agent) ++ "-" ++ (agentStatus agent) ++ "]"

isFirstOfRow :: Int -> Int -> Bool
isFirstOfRow i xLim = mod i xLim == 0

putCell :: Point -> String -> Int -> Agent -> IO ()
putCell (x,_) file idx agent
  | isFirstOfRow idx x = appendFile file $ "\n" ++ representation agent
  | otherwise          = appendFile file $ representation agent

clearScreen :: String -> IO ()
clearScreen file = appendFile file "\n\n"

get :: Game -> Int -> String -> IO ()
get (cells,dimensions) iterations file = do
  V.sequence_ $ V.imap (putCell dimensions file) cells
  clearScreen file
  --threadDelay 200000
  if iterations > 0 then get (nextState (cells,dimensions)) (iterations-1) file else return ()

nextState :: Game -> Game
nextState game = (V.map (`makeCell` game) (fst game),snd game)

makeCell :: Agent -> Game -> Agent
makeCell agent game = 
  Agent (agentType agent) (agentPoint agent) (nextAgentStatus game (filterTransitions agent) agent)
 (agentTransitions agent) (agentSight agent)

filterTransitions :: Agent -> [Game -> Agent -> Maybe Status]
filterTransitions agent = [y | (x,y) <- (agentTransitions agent), x == (agentStatus agent)]

nextAgentStatus :: Game -> [Game -> Agent -> Maybe Status] -> Agent -> Status
nextAgentStatus _ [] agent = agentStatus agent
nextAgentStatus game (f:fs) agent = let s = f game agent in
                                    if s == Nothing then nextAgentStatus game fs agent
                                    else fromJust s
