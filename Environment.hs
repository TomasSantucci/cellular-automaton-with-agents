module Environment where

import AST
import Data.List (find)
import Data.Strict.Tuple as T

-- Env data and functions definitions

initEnv :: Env
initEnv = Env [] ([] :!: 0) []

envIncludeAgent :: Agent -> Env -> Env
envIncludeAgent ag (Env sim currSim ags) = Env sim currSim (ag:ags)

envGetPreparedAgents :: Env -> [(Agent, Int)]
envGetPreparedAgents (Env _ currSim _) = T.fst currSim

findAgent :: String -> Env -> Maybe Agent
findAgent name (Env _ _ ags) = find (\ag -> (agentType ag) == name) ags

addSetAgent :: Agent -> Int -> Env -> Env
addSetAgent agent n (Env sim (setAgs :!: iters) ags) = Env sim (((agent,n):setAgs) :!: iters) ags

envSetIterations :: Int -> Env -> Env
envSetIterations n (Env sim (setAgs :!: _) ags) = Env sim (setAgs :!: n) ags

envGetIterations :: Env -> Int
envGetIterations (Env _ (_ :!: iters) _) = iters

envAddSimulation :: Simulation -> Env -> Env
envAddSimulation newSim (Env sim currSim ags) = Env (newSim:sim) currSim ags

envGetSimulations :: Env -> [Simulation]
envGetSimulations (Env sim _ _) = sim
