module Ambiant.Runner.Common
       ( runner
       ) where

import           Ambiant.Input.Parser.Brain (parseBrain)
import           Ambiant.Input.Parser.World (parseWorld)
import           Ambiant.Simulation (runSimulation, GameTrace)
import           Control.Monad (liftM)
import qualified Control.Monad.Trans.Iter as I
import           Control.Monad.Writer (Writer, runWriter)

runner :: [String] -> ([GameTrace] -> IO ()) -> IO ()
runner args stepProcessor = do
  let [worldFile,redBrainFile,blackBrainFile,roundsArg] = args
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
          let simulation = I.cutoff rounds $ runSimulation w rb bb 
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
