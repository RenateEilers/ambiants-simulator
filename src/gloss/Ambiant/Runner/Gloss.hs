module Ambiant.Runner.Gloss
       ( start )
       where

import           Ambiant.Simulation ( GameTrace(..), GameState, Pos, CellState, Color(..), 
                                      CellType(..), Ant(..), Direction(..),
                                      gsCells, cellAnt, cellType, cellFood)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (tryTakeMVar, putMVar, newEmptyMVar, MVar)
import           Control.Lens ((^.))
import           Control.Monad (forM_, void)
import qualified Data.Map as M
import           Graphics.Gloss ( blank, circleSolid, pictures, line, polygon,
                                  makeColor, 
                                  color, translate, scale, rotate,
                                  light, dark,
                                  black, red,
                                  Picture(..), Display(..), Point )
import qualified Graphics.Gloss as G
import           Graphics.Gloss.Interface.IO.Animate (animateIO)

start :: IO ([GameTrace] -> IO ())
start = do
    nextState <- newEmptyMVar

    void $ forkIO $
      animateIO (InWindow "Window Title" (600, 400) (0, 0))
                G.yellow
                (const $ renderState nextState)

    
    return $ feedTraceToState nextState

feedTraceToState :: MVar GameState -> [GameTrace] -> IO ()
feedTraceToState nextState traces = 
    forM_ traces feedState
  where
    feedState :: GameTrace -> IO ()
    feedState (TraceState gs) = putMVar nextState gs
    feedState _               = return ()

renderState :: MVar GameState -> IO Picture
renderState nextState = do
    currentState <- tryTakeMVar nextState
    return $ case currentState of
        Nothing -> blank
        Just gs -> drawState gs

drawState :: GameState -> Picture
drawState gs = 
    pictures $ map drawCell (M.toList (gs^.gsCells))

radius :: Float
radius = 10

dx :: Float
dx = radius * sqrt 3

dy :: Float
dy = 1.5 * radius

fpos :: (Int, Int) -> (Float, Float)
fpos (x,y) = (fromIntegral x, fromIntegral y)

drawCell :: (Pos, CellState) -> Picture
drawCell ((x,y), cell) =
    translate (dx*fx + if odd y then dx/2 else 0) (fy*dy)
    $ pictures [ base, food, ant ]
  where
    (fx,fy) = fpos (x,-y)

    base = pictures [ borders, fill ]
      where
        borders = color (makeColor 0.5 0.5 0.5 0.5) $ hex radius 
        fill =
            case cell^.cellType of
                Clear -> blank
                Rocky         -> color (dark $ dark G.yellow)        $ hexSolid (radius / 1.3)
                Anthill Red   -> color (light $ light $ light red)   $ hexSolid (radius / 1.3)
                Anthill Black -> color (light $ light $ light black) $ hexSolid (radius / 1.3)

    ant = case cell^.cellAnt of
        Nothing -> blank
        Just (Ant { _antColor = ac, _antFacing = af, _antHasFood = carries }) ->
            let c = case ac of Red -> red; Black -> black
                r = 90 + 60 * (case af of E -> 0; SE -> 1; SW -> 2
                                          W -> 3; NW -> 4; NE -> 5)
            in  rotate r $
                pictures $
                color c ( pictures $
                  [ translate 0 (-radius/3.5) $ circleSolid (radius/4)
                  , scale 0.3 1 $ circleSolid (radius/2)
                  , translate 0 (radius/3) $ pictures
                    [ circleSolid (radius/5)
                    , rotate (-30) $ line [(0,0), (0, radius/3)]
                    , rotate   30  $ line [(0,0), (0, radius/3)] ]
                  ] ++ legs 
                ) : [carriedFood]
          where
            legs = map (\d -> rotate d $ scale 0.5 0.5 $ line [(radius,0), (-radius,0)])
                   [-25, 0, 25]

            carriedFood = if carries
                          then translate 0 (radius/1.5) $ foodParticle 1
                          else blank

    food = foodParticle (cell^.cellFood)
        
foodParticle :: Integer -> Picture
foodParticle 0 = blank
foodParticle foodAmount =
    let pileRadius = min 0.9 (sqrt (fromIntegral foodAmount)/8.0)
    in  color (dark G.green) $ circleSolid (radius * pileRadius)

hex :: Float -> Picture
hex r = scale r r $ line hexPoints
            
hexSolid :: Float -> Picture
hexSolid r = scale r r $ polygon hexPoints

hexPoints :: [Point]
hexPoints = map (\d -> let r = d*pi/3 in (sin r, cos r)) [0..6] 
            
    
