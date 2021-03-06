module Ambiant.Input.Parser.World
       (parseWorld)
where

import           Ambiant.Input.World
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import           Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Text as T


parseCell :: P.Parser CellStart
parseCell = 
    P.choice [ P.char '#' >> return Rocky
             , P.char '.' >> return (Clear 0)
             , P.char '+' >> return (Anthill Red)
             , P.char '-' >> return (Anthill Black)
             , P.satisfy oneToNine >>= \food -> return (Clear $ fromIntegral $ digitToInt food)
             ]
    <|> (P.anyChar >>= \c -> fail $ "invalid cell character: '" ++ [c] ++ "'")
  where
    oneToNine :: Char -> Bool
    oneToNine c = c >= '1' && c <= '9'

parseRow :: Int -> P.Parser [(Int, CellStart)]
parseRow colCount = do
    cells <- P.count colCount (parseCell <* P.skipSpace)
    return $ [0..colCount] `zip` cells

parseWorld' :: P.Parser World
parseWorld' = do
    rowCount <- P.decimal <* P.endOfLine
    colCount <- P.decimal <* P.endOfLine
    rows     <- P.count rowCount $ parseRow colCount <* P.skipSpace
    return $ World (rowCount, colCount) (toCellMap $ [0..rowCount] `zip` rows)
  where 
    toCellMap :: [(Int, [(Int, CellStart)])] -> M.Map Pos CellStart
    toCellMap = M.fromList . join . map (\(row,cols) -> map (\(col, cell) -> ((col, row), cell)) cols)

parseWorld :: String -> Either String World
parseWorld worldText = P.parseOnly parseWorld' (T.pack worldText)
