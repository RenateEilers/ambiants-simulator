{-# LANGUAGE FlexibleContexts #-}
module Main where

import Ambiant.Cartography
import Ambiant.Parser.World (parseWorld)
import System.Environment


main :: IO () 
main = do
  [worldFile] <- getArgs
  worldDesc <- readFile worldFile 
  let world = parseWorld worldDesc
  print world
  
