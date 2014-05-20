{-# LANGUAGE FlexibleContexts #-}
module Main where

import Ambiant.Cartography
import Ambiant.Geography
import Ambiant.Parser.World (parseWorld)
import Ambiant.Parser.Brain (parseBrain)
import System.Environment
import Control.Monad


main :: IO () 
main = do
  [worldFile,redBrainFile,blackBrainFile] <- getArgs
  world         <- liftM parseWorld (readFile worldFile)
  redBrain      <- liftM parseBrain (readFile redBrainFile)
  blackBrain    <- liftM parseBrain (readFile blackBrainFile)
  print $ fmap mkInitialGameState world
  print redBrain
  print blackBrain
  
