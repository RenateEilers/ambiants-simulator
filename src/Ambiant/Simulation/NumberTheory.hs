module Ambiant.Simulation.NumberTheory
       ( mkNTRandom
       , ntRandom
       , NTRandom()
       ) where

import Data.List

data NTRandom = NTRandom [Int]
                   
mkNTRandom :: Integer -> NTRandom
mkNTRandom initialSeed =
    let seeds = unfoldr nextSeed initialSeed
    in  NTRandom $ map asRandom (tails seeds)
  where
    nextSeed :: Integer -> Maybe (Integer, Integer)
    nextSeed seed = let seed' = (seed * 22695477 + 1) `mod` (65536*16384)
                    in  Just (seed', seed')

    asRandom :: [Integer] -> Int
    asRandom ss = fromIntegral $ ((ss !! 3) `div` 65536) `mod` 16384
                    
ntRandom :: NTRandom -> Int -> (Int, NTRandom)
ntRandom (NTRandom (x:xs)) n = (x `mod` n, NTRandom xs)
ntRandom (NTRandom [])     _ = error "invalid NTRandom initialization"
