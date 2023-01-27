module AST where

import qualified Data.Vector as V
import Data.Strict.Tuple as T

-- Useful datatypes

type AgentName = String
type Status = String
type Path = String
type Point = (Int, Int)
type Sight = Int
type Game = (V.Vector Agent, Point) -- agents and dimensions
type Type = String

type Transitions = [(Status, Game -> Agent -> Maybe Status)]

data Agent           = Agent {
  agentType :: Type,
  agentPoint :: Point,
  agentStatus :: Status,
  agentTransitions :: Transitions,
  agentSight :: Sight
}

instance Show Agent where
  show ag = (agentType ag) ++ "-" ++ (agentStatus ag)

-- Grammar datatypes

data Neighbor
  = Neighbor Int

data Neighbors
  = Neighbors Int Int
  | AllNeighbors

data Counts
  = TypeCount AgentName Neighbors
  | StateCount Status Neighbors

data BoolExp
  = And BoolExp BoolExp
  | Or BoolExp BoolExp
  | Not BoolExp
  | EqState Neighbor Status
  | EqAgent Neighbor AgentName
  | EqCount Counts Int
  | ExpTrue
  | ExpFalse

data TransitionComm
  = Transition Status BoolExp Status
  | Seq TransitionComm TransitionComm

data Participants
  = Multi Participants Participants
  | Uni AgentName Int

-- Commands data type
data Comm
  = DefAgent AgentName Sight TransitionComm
  | SetAgent AgentName Int -- Agent and amount
--  | RemoveAgent AgentName
  | Iterations Int -- no of iterations
  | Setup Int Int String -- dimensions
--  | SetupPath Path
  | SeqComm Comm Comm
--  | Skip --unnecessary?
-- Add action command that takes place at a certain iteration

-- add another constructor to simulations that take the path to a file
data Simulation
  = Simulation [(Agent,Int)] Point Int String deriving Show -- [(agent,amount)] dimensions iterations file to save


data Env = Env [Simulation] (Pair [(Agent, Int)] Int) [Agent] deriving Show