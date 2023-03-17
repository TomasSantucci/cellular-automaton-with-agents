module Main (main) where

import AST
import Monads
import PP
import Environment
import EvalAST (evalAST)
import EvalSim (evalSim)
import Parse
import System.Console.GetOpt
import System.Environment

data Options = Options
  { optPrint    :: Bool,
    optAST      :: Bool,
    optHelp     :: Bool,
    optCellSize :: Int,
    optFPS      :: Int 
  }

defaultOptions :: Options
defaultOptions = Options {optPrint = False,
                          optAST = False,
                          optHelp = False,
                          optCellSize = 20,
                          optFPS = 2}

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p']
           ["print"]
           (NoArg (\opts -> opts { optPrint = True }))
           "Imprimir el programa de entrada."
  , Option ['a']
           ["AST"]
           (NoArg (\opts -> opts { optAST = True }))
           "Mostrar el AST del programa de entrada."
  , Option ['s']
           ["size"]
           (ReqArg (\s opts -> opts { optCellSize = read s }) "CELL-SIZE")
           "Tamaño de cada celda."
  , Option ['f']
           ["fps"]
           (ReqArg (\s opts -> opts { optFPS = read s }) "FPS")
           "FPS."
  , Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "Imprimir guia de uso."
  ]

finalOptions :: [String] -> IO Options
finalOptions argv = case getOpt Permute options argv of
  (o, _, []  ) -> return $ foldl (flip id) defaultOptions o
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Uso:"

main :: IO ()
main = do s:opts <- getArgs
          opts' <- finalOptions opts
          runOptions s opts'

runOptions :: FilePath -> Options -> IO ()
runOptions fp opts
  | optHelp opts = putStrLn (usageInfo "Uso: " options)
  | otherwise = do
    s <- readFile fp
    case sim_parse s of
      Failed e -> putStr e
      Ok ast   -> if
        | optAST opts -> print ast
        | optPrint opts -> putStrLn (renderComm ast)
        | optCellSize opts <= 0 -> ioError (userError "Tamaño de celda negativo o cero")
        | optFPS opts <= 0 -> ioError (userError "FPS negativo o cero")
        | otherwise -> runComm ast (optCellSize opts) (optFPS opts)

runComm :: Comm -> Int -> Int -> IO ()
runComm c cellSize fps
  = case stateErrorGetEnv (runStateError (evalAST c) initEnv) of
      Left e -> print e
      Right env -> evalSim cellSize fps $ envGetSimulations env