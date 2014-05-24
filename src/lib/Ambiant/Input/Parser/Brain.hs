{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Ambiant.Input.Parser.Brain
       ( parseBrain
       ) where

import           Ambiant.Input.Brain
import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import           Prelude hiding (Left, Right, drop, flip)

parseBrain :: String -> Either String AntBrain
parseBrain brainText = P.parseOnly brain (T.pack brainText)

hws :: P.Parser ()
hws = P.skipMany1 $ P.skip P.isHorizontalSpace

commentAndWs :: P.Parser ()
commentAndWs = do
    P.skipMany $ P.skip P.isHorizontalSpace
    P.choice [ P.char ';' >> P.takeTill P.isEndOfLine >> P.endOfLine >> return ()
             , void P.endOfLine
             , void P.endOfInput ]

markerId :: P.Parser MarkerId
markerId = do
    n <- P.decimal
    guard $ n >= 0 && n < 6
    return $ MarkerId n

stateId :: P.Parser BrainState
stateId = do
    n <- P.decimal
    guard $ n >= 0 && n < 10000
    return $ BrainState n

{-# ANN sense ("HLint: ignore Use liftM"::String) #-}
sense :: P.Parser Instruction
sense = do 
    P.asciiCI "Sense" >> hws
    senseDir <- P.choice [ P.asciiCI "Here" >> return Here
                         , P.asciiCI "Ahead" >> return Ahead
                         , P.asciiCI "LeftAhead" >> return LeftAhead
                         , P.asciiCI "RightAhead" >> return RightAhead
                         ] <* hws
    st1 <- stateId <* hws
    st2 <- stateId <* hws
    cond <- P.choice [ P.asciiCI "FriendWithFood" >> return FriendWithFood
                     , P.asciiCI "Friend" >> return Friend
                     , P.asciiCI "FoeHome" >> return FoeHome
                     , P.asciiCI "FoeWithFood" >> return FoeWithFood
                     , P.asciiCI "FoeMarker" >> return FoeMarker
                     , P.asciiCI "Foe" >> return Foe
                     , P.asciiCI "Food" >> return Food
                     , P.asciiCI "Rock" >> return Rock
                     , P.asciiCI "Marker" >> hws >> markerId >>= return . Marker
                     , P.asciiCI "Home" >> return Home
                     ]
    return $ Sense senseDir st1 st2 cond

mark :: P.Parser Instruction
mark = do
    P.asciiCI "Mark" >> hws
    mId <- markerId <* hws
    sId <- stateId
    return $ Mark mId sId

unmark :: P.Parser Instruction
unmark = do
    P.asciiCI "Unmark" >> hws
    mId <- markerId <* hws
    sId <- stateId
    return $ Unmark mId sId

pickup :: P.Parser Instruction
pickup = do
    P.asciiCI "PickUp" >> hws
    st1 <- stateId <* hws
    st2 <- stateId
    return $ PickUp st1 st2

drop :: P.Parser Instruction
drop = do
    P.asciiCI "Drop" >> hws
    st <- stateId
    return $ Drop st

leftOrRight :: P.Parser LeftOrRight
leftOrRight =
    P.choice [ P.asciiCI "Left"  >> return LRLeft
             , P.asciiCI "Right" >> return LRRight ]

turn :: P.Parser Instruction
turn = do
    P.asciiCI "Turn" >> hws
    lr <- leftOrRight <* hws
    st <- stateId
    return $ Turn lr st

move :: P.Parser Instruction
move = do
    P.asciiCI "Move" >> hws
    st1 <- stateId <* hws
    st2 <- stateId
    return $ Move st1 st2

flip :: P.Parser Instruction
flip = do
    P.asciiCI "Flip" >> hws
    p <- P.decimal <* hws
    guard $ p > 0
    st1 <- stateId <* hws
    st2 <- stateId
    return $ Flip p st1 st2

instruction :: P.Parser Instruction
instruction = P.choice [ sense, mark, unmark, pickup, drop, turn, move, flip ]

brain :: P.Parser
         AntBrain
brain = do
    instructions <- P.many' (instruction <* commentAndWs)
    P.endOfInput `mplus` do
        remain <- P.takeText
        fail $ "expected end of input, remaining: " ++ T.unpack remain
    return $ mkBrain instructions

