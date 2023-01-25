{-# LANGUAGE UnicodeSyntax #-}
--change arrows!!
module GameOfLifeAgents where

import           System.Environment
import           Control.Concurrent
import           Data.Maybe
import Data.Vector as V

type Game            = (Vector Agent, Point) -- agents and dimensions
type Type            = String
type Status          = Int
type Transitions     = [(Status, Game -> Maybe Status)]
type Sight           = Int

data Agent           = Agent {
  agentType :: Type,
  agentPoint :: Point,
  agentStatus :: Status,
  agentTransitions :: Transitions,
  agentSight :: Sight
}

type Point           = (Int, Int)
type AliveNeighbours = Integer

{-
prepareData ∷ [String] → Game
prepareData rawData =
  concat [ makeRow (rawData !! y) y | y ← [0..length rawData - 1]]

makeRow ∷ String → Int → [Agent]
makeRow row y =
  [((x,y), isCharLiving $ row !! x) | x ← [0..length row - 1]]

isCharLiving ∷ Char → Bool
isCharLiving char
  | char == 'o' = True
  | otherwise   = False
-}
transitions = [(0, \game -> Just 1), (1, \_ -> Just 0)]
game' = V.generate 9 (\i -> Agent "t1" (0,0) 0 transitions 1)

main ∷ IO ()
main = do
  --rawData ← readFile "./pentadecathlon"
  --get (prepareData $ lines rawData)
  get (game', (3,3))

representation ∷ Status → String
representation status = "[" ++ (show status) ++ "]"


-- See how to do the \n adding now that it's a vector
-- use function appendFile :: String -> String -> IO ()
putCell ∷ Agent → IO ()
putCell agent
  | fst (agentPoint agent) == 0 = putStr $ "\n" ++ representation (agentStatus agent)
  | otherwise           = putStr $ representation (agentStatus agent)

clearScreen ∷ IO ()
clearScreen = putStr "\ESC[2J"

iterateV :: Vector a -> 
iterateV game f = 

get ∷ Game -> Int -> IO ()
get (game,dimensions) iterations = do
  V.sequence_ V.map putCell game
  --sequence_ [putCell cell | cell ← game]
  clearScreen
  threadDelay 200000
  get (nextState game)

nextState ∷ Game → Game
nextState game = (map (`makeCell` game) (fst game),snd game)

makeCell ∷ Agent → Game → Agent
makeCell agent game = 
  Agent (agentType agent) (agentPoint agent) (nextAgentStatus game (filterTransitions agent) agent)
 (agentTransitions agent) (agentSight agent)

filterTransitions agent = [y | (x,y) <- (agentTransitions agent), x == (agentStatus agent)]

nextAgentStatus :: Game -> [Game -> Agent -> Maybe Status] -> Agent -> Status
nextAgentStatus game [] agent = agentStatus agent
nextAgentStatus game (f:fs) agent = let s = f game agent in
                                    if s == Nothing then nextAgentStatus game fs agent
                                    else fromJust s

{-
getNeighs game agent = catMaybes $ getNeighsAux game agent dirs
  where sight = agentSight agent
        dirs = [(x,y) | y <- [-sight .. sight], x <- [-sight .. sight], not ((x == 0) && (y == 0))]
        (x,y) = agentPoint agent
        getNeighsAux _ _ [] = []
        getNeighsAux game agent ((x',y'):dirs) = (getCell (x+x',y+y') game): (getNeighsAux game agent dirs)
-}

{-
nextCellState ∷ AliveNeighbours → Status → Bool
nextCellState aliveNeighbours status
  | aliveNeighbours == 3 && not status = True
  | aliveNeighbours == 2 && status     = True
  | aliveNeighbours == 3 && status     = True
  | otherwise                          = False

aliveNeighbours ∷ Game → Cell → [Point] → AliveNeighbours → AliveNeighbours
aliveNeighbours game ((x,y), status) dirs count
  | null dirs    = count
  | isAlive game (x + fst (head dirs), y + snd (head dirs))
                 = aliveNeighbours game ((x,y), status) (tail dirs) (count + 1)
  | otherwise    = aliveNeighbours game ((x,y), status) (tail dirs) count

directions ∷ [Point]
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1,0), (-1,-1)]

isAlive ∷ Game → Point → Bool
isAlive game cell
  | isNothing(getCell cell game)      = False
  | snd (fromJust(getCell cell game)) = True
  | otherwise                         = False
-}

