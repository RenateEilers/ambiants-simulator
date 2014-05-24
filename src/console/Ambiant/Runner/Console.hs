module Ambiant.Runner.Console
       ( renderTraces )
       where 

import           Ambiant.Simulation
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Maybe


render :: GameState -> String
render gs =
    let rows = do y <- [0..maxy]
                  return $ renderRow y
    in  intercalate "\n" rows
  where
    (maxx, maxy) = maximum $ M.keys (gs^.gsCells)

    renderRow :: Int -> String
    renderRow y = let cells = do x <- [0..maxx]
                                 return $ renderCell x y
                      prefix = if odd y then " " else ""
                  in  prefix ++ unwords cells

    renderCell :: Int -> Int -> String
    renderCell x y = let cell = fromJust $ gs^.gsCells.at (x,y)
                     in  [cellChar cell]

    viewCell :: CellState -> (Maybe (Color, Direction, Bool), (Integer, CellType))
    viewCell cell = (fmap (\a-> (a^.antColor, a^.antFacing, a^.antHasFood)) (cell^.cellAnt),
                     (cell^.cellFood, cell^.cellType))

    cellChar :: CellState -> Char
    cellChar cell = case viewCell cell of
        (Just (Red,   _, _), _)                     -> 'O'
        (Just (Black, _, _), _)                     -> '@'
        (Nothing           , (0   , Rocky))         -> '#'
        (Nothing           , (0   , Anthill Red))   -> '-'
        (Nothing           , (0   , Anthill Black)) -> '+'
        (Nothing           , (0   , Clear))         -> '.'
        (Nothing           , (food, _))             -> Data.Char.chr (fromInteger (min 57 (food+48)))

renderTraces :: [GameTrace] -> IO ()
renderTraces traces = forM_ traces renderTrace
  where
    renderTrace :: GameTrace -> IO ()
    renderTrace trace = case trace of
        TraceMessage _   -> return ()
        TraceState state -> putStrLn $ render state
