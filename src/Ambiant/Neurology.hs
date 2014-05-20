module Ambiant.Neurology
       ( BrainState(..)
       , MarkerId(..)
       , SenseDirection(..)
       , SenseCondition(..)
       , LeftOrRight(..)
       , Instruction(..)
       , AntBrain
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
                 
data LeftOrRight = Left | Right
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

newtype AntBrain = AntBrain (M.Map BrainState Instruction)
                 deriving (Show, Eq)


mkBrain :: [Instruction] -> AntBrain
mkBrain instructions =
    AntBrain $ M.fromList $ (map BrainState [0..]) `zip` instructions 
    
