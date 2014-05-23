module Ambiant.Input.Brain
       ( BrainState(..)
       , MarkerId(..)
       , SenseDirection(..)
       , SenseCondition(..)
       , LeftOrRight(..)
       , Instruction(..)
       , AntBrain(..)
       , mkBrain
       ) where

import qualified Data.Map as M

newtype BrainState = BrainState Int
                   deriving (Show, Eq, Ord)

newtype MarkerId = MarkerId Int
                 deriving (Show, Eq, Ord)

data SenseDirection = Here
                    | Ahead
                    | LeftAhead
                    | RightAhead
                    deriving (Show, Eq)

data SenseCondition = Friend
                    | Foe
                    | FriendWithFood
                    | FoeWithFood
                    | Food
                    | Rock
                    | Marker MarkerId
                    | FoeMarker
                    | Home
                    | FoeHome
                    deriving (Show, Eq)
                 
data LeftOrRight = LRLeft | LRRight
                 deriving (Show, Eq)

data Instruction = Sense SenseDirection BrainState BrainState SenseCondition
                 | Mark MarkerId BrainState
                 | Unmark MarkerId BrainState
                 | PickUp BrainState BrainState
                 | Drop BrainState
                 | Turn LeftOrRight BrainState
                 | Move BrainState BrainState
                 | Flip Int BrainState BrainState
                 deriving (Show, Eq)

newtype AntBrain = AntBrain { instructionOf :: BrainState -> Maybe Instruction }

mkBrain :: [Instruction] -> AntBrain
mkBrain instructions = let brainMap = M.fromList $ map BrainState [0..] `zip` instructions 
                       in  AntBrain $ flip M.lookup brainMap
