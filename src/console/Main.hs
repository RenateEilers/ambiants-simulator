{-# LANGUAGE FlexibleContexts #-}
module Main where

import Ambiant.Runner.Common (runner)
import Ambiant.Runner.Console (renderTraces)
import System.Environment (getArgs)

main :: IO () 
main = do
    args <- getArgs
    runner args renderTraces
