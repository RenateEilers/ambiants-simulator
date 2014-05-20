{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Ambiant.Parser.Brain
       ( parseBrain
       ) where

import           Ambiant.Neurology
import           Control.Applicative
import           Control.Monad.Error
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import           Prelude hiding (Left, Right, drop, flip)

parseBrain :: String -> Either String AntBrain
parseBrain brainText = P.parseOnly brain (T.pack brainText)

hws :: P.Parser ()
hws = P.skipMany1 $ P.skip P.isHorizontalSpace

skipCommentAndWhitespace :: P.Parser ()
skipCommentAndWhitespace = do
    P.skipMany $ P.skip P.isHorizontalSpace
    P.choice [ P.char ';' >> P.takeTill P.isEndOfLine >> P.endOfLine >> return ()
             , P.endOfLine >> return ()
             , P.endOfInput >> return () ]

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

sense :: P.Parser Instruction
sense = do 
    P.string "Sense" >> hws
    senseDir <- P.choice [ P.string "Here" >> return Here
                         , P.string "Ahead" >> return Ahead
                         , P.string "LeftAhead" >> return LeftAhead
                         , P.string "RightAhead" >> return RightAhead
                         ] <* hws
    st1 <- stateId <* hws
    st2 <- stateId <* hws
    cond <- P.choice [ P.string "Friend" >> return Friend
                     , P.string "Foe" >> return Foe
                     , P.string "FriendWithFood" >> return FriendWithFood
                     , P.string "FoeWithFood" >> return FoeWithFood
                     , P.string "Food" >> return Food
                     , P.string "Rock" >> return Rock
                     , P.string "Marker" >> hws >> markerId >>= return . Marker
                     , P.string "Home" >> return Home
                     , P.string "FoeHome" >> return FoeHome
                     ]
    return $ Sense senseDir st1 st2 cond

mark :: P.Parser Instruction
mark = do
    P.string "Mark" >> hws
    mId <- markerId <* hws
    sId <- stateId
    return $ Mark mId sId

unmark :: P.Parser Instruction
unmark = do
    P.string "Unmark" >> hws
    mId <- markerId <* hws
    sId <- stateId
    return $ Unmark mId sId

pickup :: P.Parser Instruction
pickup = do
    P.string "PickUp" >> hws
    st1 <- stateId <* hws
    st2 <- stateId
    return $ PickUp st1 st2

drop :: P.Parser Instruction
drop = do
    P.string "Drop" >> hws
    st <- stateId
    return $ Drop st

leftOrRight :: P.Parser LeftOrRight
leftOrRight =
    P.choice [ P.string "Left" >> return Left
             , P.string "Right" >> return Right ]

turn :: P.Parser Instruction
turn = do
    P.string "Turn" >> hws
    lr <- leftOrRight <* hws
    st <- stateId
    return $ Turn lr st

move :: P.Parser Instruction
move = do
    P.string "Move" >> hws
    st1 <- stateId <* hws
    st2 <- stateId
    return $ Move st1 st2

flip :: P.Parser Instruction
flip = do
    P.string "Flip" >> hws
    p <- P.decimal <* hws
    guard $ p > 0
    st1 <- stateId <* hws
    st2 <- stateId
    return $ Flip p st1 st2

instruction :: P.Parser Instruction
instruction = P.choice [ sense, mark, unmark, pickup, drop, turn, move, flip ]

brain :: P.Parser AntBrain
brain = do
    instructions <- P.many' (instruction <* skipCommentAndWhitespace)
    P.endOfInput `mplus` do
        remain <- P.takeText
        fail $ "expected end of input, remaining: " ++ (T.unpack remain)
    return $ mkBrain instructions

