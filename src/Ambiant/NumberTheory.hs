module Ambiant.NumberTheory where

import Data.List
import System.Random

newtype NTRandom = NTRandom [Integer]
                   
mkNTRandom :: Integer -> NTRandom
mkNTRandom s = NTRandom $ unfoldr next s
  where
    next :: Integer -> Maybe (Integer, Integer)
    next s = let s' = s * 22695477 + 1
             in  Just (s', s')

instance RandomGen NTRandom where
    genRange _ = (0, 16383)

    next (NTRandom ss) =
        let ss' = tail ss
            x   = fromIntegral $ ((ss !! 3) `div` 65536) `mod` 16384
        in  (x, NTRandom ss')

    split (NTRandom (s:ss)) = (mkNTRandom $ s, NTRandom $ ss)

newtype NTInt = NTInt Int
              deriving (Show, Eq, Ord)

instance Random NTInt where
    randomR (NTInt lo, NTInt hi) g = let (ai, g') = randomR (lo, hi) g
                                     in  (NTInt ai, g')

    random g = let (ai, g') = next g
               in  (NTInt $ ai `mod` 16384, g')
                    
