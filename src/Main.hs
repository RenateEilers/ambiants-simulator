{-# LANGUAGE FlexibleContexts #-}
module Main where

import Ambiant.Input (parseBrain, parseWorld)
import Ambiant.Runner.Console
import Ambiant.Simulation (runSimulation)
import Control.Monad
import Control.Monad.Trans.Iter as I
import Control.Monad.Writer
import System.Environment

main :: IO () 
main = do
  [worldFile,redBrainFile,blackBrainFile,roundsArg] <- getArgs
  world         <- liftM parseWorld (readFile worldFile)
  redBrain      <- liftM parseBrain (readFile redBrainFile)
  blackBrain    <- liftM parseBrain (readFile blackBrainFile)
  
  let parsed = do
          w <- world
          rb <- redBrain
          bb <- blackBrain
          return (w,rb,bb)

      rounds = read roundsArg

  case parsed of 
      Left err -> putStrLn $ "Error while parsing input: " ++ err
      Right (w,rb,bb) -> do
          let simulation = cutoff rounds $ runSimulation w rb bb 
              stepProcessor = renderTraces 
              step = takeStep stepProcessor
          result <- I.foldM step simulation
          case result of
              Nothing -> putStrLn $ "Simulation still running after " ++ show rounds ++ " rounds"
              Just (Left err) -> putStrLn $ "Error during simulation: " ++ err
              Just (Right traces) -> do stepProcessor traces
                                        putStrLn "Simulation ended"

takeStep :: (Monad m) => (w -> m ()) -> Writer w (m a) -> m a
takeStep k step = let (a, w) = runWriter step
                  in  k w >> a

