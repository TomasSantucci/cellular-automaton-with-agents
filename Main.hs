module Main (main) where

import AST
import Agents
import Monads
import GameOfLifeAgents (runSims, simsToGrids)
import Environment
import Eval (eval)
import Parse
import Text.Read (readMaybe)
import System.Environment

main :: IO ()
main = do args <- getArgs
          (s,(cellSize, speed)) <- parseArgs args
          r <- readFile s
          case sim_parse r of
            Ok command -> execComm command cellSize speed
            Failed e -> putStr e

parseArgs :: [String] -> IO (String,(Int,Int))
parseArgs args = case args of
                   [s] -> return (s,(20,3))
                   [s,cellSize] -> do r <- readArg cellSize
                                      return (s,(r,3))
                   [s,cellSize,speed] -> do r1 <- readArg cellSize
                                            r2 <- readArg speed
                                            return (s,(r1,r2))
                   _ -> ioError $ userError "Error parsing arguments"

readArg :: String -> IO Int
readArg arg = case readMaybe arg of
                Just n -> return n
                Nothing -> ioError $ userError "Error parsing argument"

execComm :: Comm -> Int -> Int -> IO ()
execComm c cellSize speed
  = case stateErrorGetEnv (runStateError (eval c) initEnv) of
      Left e -> print e
      Right env -> do sims <- simsToGrids $ envGetSimulations env
                      runSims sims cellSize speed