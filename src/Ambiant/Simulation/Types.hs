{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Ambiant.Simulation.Types
       ( module Ambiant.Simulation.Types
       , module Ambiant.Input.Brain 
       , module Ambiant.Input.World
       )
where

import           Ambiant.Input.Brain ( AntBrain, MarkerId, BrainState(..) )
import           Ambiant.Input.World ( Color(..), Pos)
import           Ambiant.Simulation.NumberTheory
import           Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import           Prelude hiding (round)

newtype AntId = AntId Int
              deriving (Eq, Show, Ord, Bounded, Enum)

data Direction = E | SE | SW | W | NW | NE
            deriving (Eq, Show, Ord, Bounded, Enum)

data Ant = Ant { _antId :: AntId
               , _antColor :: Color
               , _antFacing :: Direction
               , _antResting :: Int
               , _antHasFood :: Bool
               , _antState :: BrainState
               } deriving (Show)

mkAnt :: AntId -> Color -> Ant
mkAnt i c = Ant i c E 0 False (BrainState 0)

data CellType = Clear | Rocky | Anthill Color
              deriving (Show, Eq)

data CellState = CellState
                 { _cellType :: CellType
                 , _cellAnt :: Maybe Ant
                 , _cellMarkers :: S.Set (Color, MarkerId)
                 , _cellFood :: Integer
                 } deriving (Show)

data GameState = GameState
                 { _gsCells :: M.Map Pos CellState
                 , _gsRound :: Int
                 , _gsRandom :: NTRandom
                 , _gsAntPos :: M.Map AntId Pos
                 }

data GameConfig = GameConfig
                  { _gcBrain :: Color -> AntBrain
                  }

data GameTrace = TraceMessage String
               | TraceState GameState

makeLenses ''Ant
makeLenses ''CellState
makeLenses ''GameState
makeLenses ''GameConfig
