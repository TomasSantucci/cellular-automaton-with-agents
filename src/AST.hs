module AST where

import qualified Data.Vector as V
import Data.Strict.Tuple as T
import Graphics.Gloss

type AgentName = String
type State = String
type Path = String
type MyPoint = (Int, Int)
type Game = (V.Vector Agent, MyPoint)
type Rule = (State, Game -> Agent -> Maybe Result)
data Env = Env [Simulation] (Pair [(Agent, Int)] Int) [Agent]
type Model = ((Game, Int), Int)

data Simulation
  = Simulation [(Agent,Int)] MyPoint Int
  | SimulationPath String Int [(Agent, Int)]

data Agent = Agent {
  agentType :: String,
  agentPoint :: MyPoint,
  agentState :: State,
  agentColors :: [(State,Color)],
  agentRules :: [Rule],
  agentSight :: Int,
  agentAttributes :: [(String, Int)]
}

type Neighbor = Int

data Neighbors
  = Neighbors Int Int
  | AllNeighbors

type Result = Either State (String, Int)
type UnparsedResult = Either State (String, IntExp)

data IntExp
  = Const Int
  | TypeCount AgentName Neighbors
  | StateCount State Neighbors
  | Att String
  | Plus IntExp IntExp
  | Minus IntExp IntExp
  | Div IntExp Int
  | Times IntExp IntExp

data BoolExp
  = And BoolExp BoolExp
  | Or BoolExp BoolExp
  | Not BoolExp
  | EqState Neighbor State
  | EqAgent Neighbor AgentName
  | Eq IntExp IntExp
  | Lt IntExp IntExp
  | Gt IntExp IntExp
  | ExpTrue
  | ExpFalse

data RulesComm
  = DefRule State BoolExp UnparsedResult
  | Seq RulesComm RulesComm

data Attributes
  = NoAtt
  | Attribute String Int
  | SeqAtt Attributes Attributes

data MyColor
  = ColorName String
  | ColorMake Int Int Int Int

data StatesComm
  = DefState State MyColor
  | SeqSt StatesComm StatesComm

data Comm
  = DefAgent AgentName Int Attributes StatesComm RulesComm
  | SetAgent AgentName Int
  | UnsetAgent AgentName
  | Iterations Int
  | Setup Int Int
  | SetupPath String
  | SeqComm Comm Comm