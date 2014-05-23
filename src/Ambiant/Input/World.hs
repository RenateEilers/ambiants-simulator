module Ambiant.Input.World
       where

import qualified Data.Map as M

data Color = Black | Red
           deriving (Show, Eq, Ord)

data CellStart = Clear Integer
               | Rocky
               | Anthill Color
               deriving (Show, Eq)

type Pos = (Int, Int)
type Size = (Int, Int)
data World = World
             { wSize :: Size
             , wCells :: M.Map Pos CellStart
             } deriving (Show, Eq)

