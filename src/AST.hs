module AST where

import qualified Data.Vector as V
import Data.Strict.Tuple as T
import Graphics.Gloss

type AgentName = String
type State = String
type MyPoint = (Int, Int)
type Grid = (V.Vector Agent, MyPoint)
type Rule = Grid -> Agent -> Maybe (Result Int)
data Env = Env [Simulation] (Pair [(Agent, Int)] Int) [Agent]

data Simulation
  = Simulation [(Agent,Int)] MyPoint Int
  | SimulationPath String Int [(Agent, Int)]

data Agent = Agent {
  agentType :: String,
  agentPoint :: MyPoint,
  agentState :: State,
  agentColors :: [(State,Color)],
  agentRules :: [(State,Rule)],
  agentSight :: Int,
  agentAttributes :: [(String, Int)]
}

type Neighbor = Int

data Neighbors
  = Neighbors Int Int
  | AllNeighbors
  deriving Show

data Result t
  = NewState State
  | ChangeAttribute String t
  deriving Show

data Exp a where
  -- Int
  Const :: Int -> Exp Int
  TypeCount :: AgentName -> Neighbors -> Exp Int
  StateCount :: State -> Neighbors -> Exp Int
  Att :: String -> Exp Int
  Plus :: Exp Int -> Exp Int -> Exp Int
  Minus :: Exp Int -> Exp Int -> Exp Int
  Times :: Exp Int -> Exp Int -> Exp Int
  Div :: Exp Int -> Int -> Exp Int
  -- Bool
  ExpTrue :: Exp Bool
  ExpFalse :: Exp Bool
  Lt :: Exp Int -> Exp Int -> Exp Bool
  Gt :: Exp Int -> Exp Int -> Exp Bool
  And :: Exp Bool -> Exp Bool -> Exp Bool
  Or :: Exp Bool -> Exp Bool -> Exp Bool
  Not :: Exp Bool -> Exp Bool
  Eq :: Exp Int -> Exp Int -> Exp Bool
  EqState :: Neighbor -> State -> Exp Bool
  EqAgent :: Neighbor -> AgentName -> Exp Bool

deriving instance Show (Exp a)

data RulesComm
  = DefRule State (Exp Bool) (Result (Exp Int))
  | Seq RulesComm RulesComm
  deriving Show

data Attributes
  = NoAtt
  | Attribute String Int
  | SeqAtt Attributes Attributes
  deriving Show

data MyColor
  = ColorName String
  | ColorMake Int Int Int
  deriving Show

data StatesComm
  = DefState State MyColor
  | SeqSt StatesComm StatesComm
  deriving Show

data Comm
  = DefAgent AgentName Int Attributes StatesComm RulesComm
  | SetAgent AgentName Int
  | UnsetAgent AgentName
  | Iterations Int
  | Setup Int Int
  | SetupPath String
  | SeqComm Comm Comm
  deriving Show