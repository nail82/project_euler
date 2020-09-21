module Prob3 where

import Data.Vector as V
import Data.Vector.Generic as G
import Data.Vector.Generic.Mutable as GM

{-
  What is the largest prime factor of the number 600851475143?
-}

bigOne :: Int
bigOne = 600851475143

smallOne :: Int
smallOne = 13195

run3 :: IO ()
run3 = putStrLn "Problem 3 => Not Solved"

data Mark =
    Unmarked | Marked
    deriving (Eq, Show)

{-
I was trying a newtype here to fool around with it.  It
started getting unwieldy so I dropped back to a tuple.

newtype MyNum =
    MyNum (Int, Mark)
    deriving (Eq, Show)
-}

markIt :: (Int, Mark) -> (Int, Mark)
markIt (x,_) = (x,Marked)

-- sieve :: Int -> Int -> V.Vector (Int, Mark) -> V.Vector (Int, Mark)
-- sieve maxN i ms
--       | 2*i > maxN = ms
--       | otherwise =
--           let m = div maxN i
--               idxs = V.enumFromStepN i i m
--           in markEm idxs ms

makeMyNum :: Int -> (Int, Mark)
makeMyNum n = (n, Unmarked)


-- This what I'd like
sieve' :: V.Vector (Int, Mark) -> V.Vector Int
sieve' = undefined

-- A single iteration of marking
markEm :: Int -> V.Vector (Int, Mark) -> V.Vector (Int, Mark)
markEm = undefined
