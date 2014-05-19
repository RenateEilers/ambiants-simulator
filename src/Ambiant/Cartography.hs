module Ambiant.Cartography
       ( Color(..)
       , Cell(..)
       , Pos
       , Size
       , World(..)
       )
where
  
import qualified Data.Map as M

data Color = Black | Red
           deriving (Show, Eq)

data Cell = Clear Int
          | Rocky
          | Anthill Color
          deriving (Show, Eq)

type Pos = (Int, Int)
type Size = (Int, Int)
data World = World Size (M.Map Pos Cell)
           deriving (Show, Eq)
