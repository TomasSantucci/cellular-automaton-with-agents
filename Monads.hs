module Monads where

import AST
import Data.Strict.Tuple
import Environment
import Control.Monad                  ( liftM
                                      , ap
                                      )

class Monad m => MonadState m where
    addAgent :: Agent -> m Agent
    getAgents :: m [(Agent, Int)]
    setAgent :: String -> Int -> m ()
    setIterations :: Int -> m ()
    getIterations :: m Int
    addSimulation :: Simulation -> m ()

class Monad m => MonadError m where
    throw :: String -> m a

-- Monad definition and instances

newtype StateError a = StateError {runStateError :: Env -> Either String (Pair a Env)}

instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case runStateError m s of
                                     Left e -> Left e
                                     Right (v :!: s') -> runStateError (f v) s')

instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

instance MonadState StateError where
  addAgent ag = StateError (\s -> Right (ag :!: (envIncludeAgent ag s)))

  getAgents = StateError (\s -> Right ((envGetPreparedAgents s) :!: s))

  setAgent name n = StateError (\s -> case findAgent name s of
                                        Nothing -> Left "Undef Agent"
                                        Just ag -> Right (() :!: (addSetAgent ag n s)))

  setIterations n = StateError (\s -> Right (() :!: (envSetIterations n s)))

  getIterations = StateError (\s -> Right ((envGetIterations s) :!: s))

  addSimulation sim = StateError (\s -> Right (() :!: (envAddSimulation sim s)))

stateErrorGetEnv :: Either String (Pair a Env) -> Either String Env
stateErrorGetEnv (Right (_ :!: env)) = Right env
stateErrorGetEnv (Left e) = Left e
