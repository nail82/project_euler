module Prob3 where

import Data.Vector

{-
  What is the largest prime factor of the number 600851475143?
-}

bigOne = 600851475143
smallOne = 13195

run3 :: IO ()
run3 = putStrLn "Problem 3 => Not Solved"

data Mark =
    Unmarked | Marked
    deriving (Eq, Show)

{-
I was trying a newtype hear to fool around with it.  It
started getting unwieldy so I dropped back to a tuple.

newtype MyNum =
    MyNum (Int, Mark)
    deriving (Eq, Show)
-}

markIt :: (Int, Mark) -> (Int, Mark)
markIt (x,_) = (x,Marked)

sieve :: Int -> Vector (Int, Mark) -> Vector (Int, Mark)
sieve i ms =
    let (x,_) = ms ! i

    in ms

makeMyNum :: Int -> (Int, Mark)
makeMyNum n = (n, Unmarked)
