module GameOfLifeAgents where

import AST
import           Control.Concurrent
import Graphics.Gloss
import System.Random (newStdGen, mkStdGen, StdGen)
import           Data.Maybe
import Data.Vector as V (sequence_, map, imap, toList, empty, null)

cell2pic :: Agent -> Picture
cell2pic agent = rectMoved
  where (x,y) = agentPoint agent
        stName = agentStatus agent
        dimension = 20
        col = case lookup stName (agentColors agent) of
                Nothing -> greyN 0.5
                Just c -> c
        rectangle = rectangleSolid (fromIntegral dimension) (fromIntegral dimension)
        rectColored = color col rectangle
        rectMoved = translate (fromIntegral (x*dimension)) (fromIntegral (y*dimension)) rectColored

cells2pic (cells,_) = pictures $ V.toList $ V.map cell2pic cells

getRngs :: Int -> IO ([StdGen])
getRngs 1 = do rng <- newStdGen
               return [rng]
getRngs n = do rng <- newStdGen
               rngs <- getRngs (n-1)
               return $ (rng:rngs)

applyRngs :: Int -> [a] -> [a -> b] -> [(b,Int)]
applyRngs n [] [] = []
applyRngs n (x:xs) (f:fs) = (f x, n):(applyRngs (n+1) xs fs)

emptyGame :: Game
emptyGame = (V.empty, (0,0))

isEmptyGame :: Game -> Bool
isEmptyGame (cells,_) = V.null cells

startScreen n = pictures [pic2,pic1]
  where pic1 = color white $ Translate (-125) (0) $ Scale 0.3 0.3 $ Text ("simulacion " ++ (show n))
        pic2 = Translate (-10) 10 $ color black $ rectangleSolid 300 100

get :: [StdGen -> (Game, Int)] -> IO ()
get xs =
  do rngs <- getRngs (length xs)
     let listOfGames = ((emptyGame,0),0):(applyRngs 0 rngs xs)
     simulate screen white 1 listOfGames g (\_ _ model -> f model)
  where screen = InWindow "game" (500,500) (500,500)
        g (((game,iters),n):xs) = if isEmptyGame game then startScreen n else cells2pic game
        f [((game,0),_)] = [((game,0),0)]
        f ((((cells,dim),it),n):rest) | V.null cells = rest
                                      | it == 0 = ((emptyGame,0),n+1):rest
                                      | otherwise = (((nextState (cells,dim)),it-1),n):rest

nextState :: Game -> Game
nextState game = (V.map (`makeCell` game) (fst game),snd game)

makeCell :: Agent -> Game -> Agent
makeCell (Agent type point status colors rules sight atts) game =
  Agent type point (nextAgentStatus game (filterTransitions agent) agent) colors rules sight atts 

filterTransitions :: Agent -> [Game -> Agent -> Maybe Result]
filterTransitions agent = [y | (x,y) <- (agentTransitions agent), x == (agentStatus agent)]

updateList (s,v) [] = [(s,v)]
updateList (s,v) ((s',v'):xs) = if s == s' then (s,v):xs
                                           else (s',v'):(updateList (s,v) xs)

updateAtt :: Agent -> (String, Int) -> Agent
updateAtt (Agent n p st cols t s atts) att = Agent n p st cols t s (updateList att atts)

nextAgentStatus :: Game -> [Game -> Agent -> Maybe Result] -> Agent -> Status
nextAgentStatus _ [] agent = agentStatus agent
nextAgentStatus game (f:fs) agent = case f game agent of
                                      Nothing -> nextAgentStatus game fs agent
                                      Just (Left st) -> st
                                      Just (Right att) -> nextAgentStatus game fs (updateAtt agent att)
