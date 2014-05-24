module Main where

import Ambiant.Runner.Common (runner)
import Ambiant.Runner.Gloss (start)
import System.Environment (getArgs)

main :: IO () 
main = do
    processor <- start
    args <- getArgs
    runner args processor
