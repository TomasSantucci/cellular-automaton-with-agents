module AST where

import qualified Data.Vector as V
import Data.Strict.Tuple as T
import Graphics.Gloss

-- Useful datatypes

type AgentName = String
type Status = String
type Path = String
type MyPoint = (Int, Int)
type Sight = Int
type Game = (V.Vector Agent, MyPoint) -- agents and dimensions
type Type = String

type Transitions = [(Status, Game -> Agent -> Maybe Result)]

data Agent           = Agent {
  agentType :: Type,
  agentPoint :: MyPoint,
  agentStatus :: Status,
  agentColors :: [(Status,Color)],
  agentTransitions :: Transitions,
  agentSight :: Sight,
  agentAttributes :: [(String, Int)]
}

instance Show Agent where
  show ag = (agentType ag) ++ "-" ++ (agentStatus ag)

-- Grammar datatypes

type Neighbor = Int

data Neighbors
  = Neighbors Int Int
  | AllNeighbors
  deriving Show

data IntExp
  = Const Int
  | TypeCount AgentName Neighbors
  | StateCount Status Neighbors
  | Att String
  | Plus IntExp IntExp
  | Minus IntExp IntExp
  | Div IntExp Int
  | Times IntExp IntExp
  deriving Show

data BoolExp
  = And BoolExp BoolExp
  | Or BoolExp BoolExp
  | Not BoolExp
  | EqState Neighbor Status
  | EqAgent Neighbor AgentName
  | Eq IntExp IntExp
  | Lt IntExp IntExp
  | Gt IntExp IntExp
  | ExpTrue
  | ExpFalse
  deriving Show

type Result = Either Status (String, Int)
type UnparsedResult = Either Status (String, IntExp)

data TransitionComm
  = Transition Status BoolExp UnparsedResult
  | Seq TransitionComm TransitionComm
  deriving Show

data Participants
  = Multi Participants Participants
  | Uni AgentName Int

data Attributes
  = NoAtt
  | Attribute String Int
  | SeqAtt Attributes Attributes
  deriving Show

data MyColor
  = ColorName String
  | ColorMake Int Int Int Int
  deriving Show

data States
  = State Status MyColor
  | SeqSt States States
  deriving Show

-- Commands data type
data Comm
  = DefAgent AgentName Sight Attributes States TransitionComm
  | SetAgent AgentName Int -- Agent and amount
  | UnsetAgent AgentName
  | Iterations Int -- no of iterations
  | Setup Int Int -- dimensions
  | SetupPath String
  | SeqComm Comm Comm
  deriving Show

-- add another constructor to simulations that take the path to a file
data Simulation
  = Simulation [(Agent,Int)] MyPoint Int -- [(agent,amount)] dimensions iterations file to save
  | SimulationPath String Int [(Agent, Int)]
  deriving Show

cellSize :: Int
cellSize = 30

data Env = Env [Simulation] (Pair [(Agent, Int)] Int) [Agent] deriving Show