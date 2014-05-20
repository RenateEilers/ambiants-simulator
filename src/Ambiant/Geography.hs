module Ambiant.Geography
       ( mkInitialGameState )
       where

import           Ambiant.Cartography hiding ( CellStart(..) )
import qualified Ambiant.Cartography as C
import           Ambiant.Neurology
import qualified Data.Map as M
import qualified Data.Set as S

newtype AntId = AntId Int
              deriving (Eq, Show, Ord)

data AntDir = E | SE | SW | W | NW | NE
            deriving (Eq, Show, Ord, Enum)

data Ant = Ant { _id :: AntId
               , _color :: Color
               , _pos :: Pos
               , _dir :: AntDir
               , _resting :: Int
               } deriving (Show)

mkAnt :: AntId -> Color -> Pos -> Ant
mkAnt i c p = Ant i c p E 0

data CellType = Clear | Rocky | Anthill Color
              deriving (Show)

data CellState = CellState
                 { _celltype :: CellType
                 , _ant :: Maybe Ant
                 , _markers :: S.Set (Color, MarkerId)
                 , _food :: Integer
                 } deriving (Show)

data GameState = GameState (M.Map Pos CellState)
               deriving (Show)

mkInitialGameState :: World -> GameState
mkInitialGameState (World (sx, sy) initialCells) =
    GameState $ fst $ M.foldlWithKey' mkCell (M.empty, AntId 0) initialCells
  where
    mkCell :: (M.Map Pos CellState, AntId) -> Pos -> C.CellStart -> (M.Map Pos CellState, AntId)
    mkCell (gs, nid)           pos C.Rocky           = (M.insert pos (CellState Rocky Nothing S.empty 0) gs, nid)
    mkCell (gs, nid)           pos (C.Clear f)       = (M.insert pos (CellState Clear Nothing S.empty f) gs, nid)
    mkCell (gs, nid@(AntId i)) pos (C.Anthill color) = (M.insert pos (CellState
                                                                      (Anthill color)
                                                                      (Just $ mkAnt nid color pos) S.empty 0) gs
                                                                , AntId (i+1))
