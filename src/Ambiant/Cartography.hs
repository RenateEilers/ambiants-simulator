module Ambiant.Cartography
       ( Color(..)
       , CellStart(..)
       , Pos
       , Size
       , World(..)
       )
where
  
import qualified Data.Map as M

data Color = Black | Red
           deriving (Show, Eq)

data CellStart = Clear Integer
          | Rocky
          | Anthill Color
          deriving (Show, Eq)

type Pos = (Int, Int)
type Size = (Int, Int)
data World = World Size (M.Map Pos CellStart)
           deriving (Show, Eq)
