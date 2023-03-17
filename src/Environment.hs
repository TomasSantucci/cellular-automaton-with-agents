module Environment where

import AST
import Data.List (find)
import Data.Strict.Tuple as T

initEnv :: Env
initEnv = Env [] ([] :!: 0) []

envIncludeAgent :: Agent -> Env -> Env
envIncludeAgent ag (Env sim currSim ags) = Env sim currSim (ag:ags)

envGetPreparedAgents :: Env -> [(Agent, Int)]
envGetPreparedAgents (Env _ currSim _) = T.fst currSim

envFindAgent :: String -> Env -> Maybe Agent
envFindAgent name (Env _ _ ags) = find (\ag -> (agentType ag) == name) ags

envAddSetAgent :: Agent -> Int -> Env -> Env
envAddSetAgent agent n (Env sim (setAgs :!: iters) ags) = Env sim (((agent,n):setAgs) :!: iters) ags

removeItem :: (a -> Bool) -> [a] -> [a]
removeItem _ [] = []
removeItem cmp (x:xs) = if cmp x then xs else x : (removeItem cmp xs)

envUnsetAgent :: String -> Env -> Env
envUnsetAgent name (Env sim (setAgs :!: iters) ags)
  = Env sim ((removeItem (\(ag,_) -> (agentType ag) == name) setAgs) :!: iters) ags

envSetIterations :: Int -> Env -> Env
envSetIterations n (Env sim (setAgs :!: _) ags) = Env sim (setAgs :!: n) ags

envGetIterations :: Env -> Int
envGetIterations (Env _ (_ :!: iters) _) = iters

envAddSimulation :: Simulation -> Env -> Env
envAddSimulation newSim (Env sim currSim ags) = Env (newSim:sim) currSim ags

envGetSimulations :: Env -> [Simulation]
envGetSimulations (Env sim _ _) = sim
