
module Prob7 where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!), (//), (++))
--import Control.Monad.Trans.State

{-
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can
  see that the 6th prime is 13.

  What is the 10 001st prime number?
-}

{-
  Some thoughts after reading wikipedia:
  - Run a sieve out to something manageable and store them
  - Check against the sieve values
  - Then check [sieve + 1 to sqrt(n)]
-}

run7 :: IO ()
run7 = do
  putStr "Problem 7 => "
  putStrLn "unsolved"


ans :: Int
ans = undefined

croot = ceiling . sqrt


nthHelper :: Int -> Int
nthHelper n = undefined

findNextPrime :: Int -> Int -> U.Vector Int -> U.Vector Int
-- p is the candiate prime
-- i is the index where we'll park the new prime
-- k is the current index we're checking
findNextPrime p i v = undefined

outerloop :: Int -> U.Vector Int -> Int
outerloop p v =
    let stop = croot $ fromIntegral p
        k = 0
        p' = innerloop p k stop v
    in case p' > 0 of
         True -> p'
         False -> outerloop (p + 1) v


innerloop :: Int -> Int -> Int -> U.Vector Int -> Int
innerloop p k stop v =
    case v ! k > stop of
      True -> p
      False -> case p `rem` (v ! k) == 0 of
                 True -> 0
                 False -> innerloop p (k + 1) stop v




nthPrime :: Int -> Int
nthPrime n
    | n < 1 = 1
    | otherwise = let f670 = first670
                  in case (n - 1) < U.length f670 of
                       True -> f670 ! (n - 1)
                       False -> nthHelper n

-- Generate the first few primes
sieve :: U.Vector Int -> U.Vector Int
sieve v =
    let loop i v =
            case i^2 < (U.length v) of
              True -> let v' = sieveHelper i v
                      in loop (i+1) v'
              False -> v
    in U.filter (>1) $ loop 2 v

sieveHelper :: Int -> U.Vector Int -> U.Vector Int
sieveHelper k v =
    let end = U.length v
        loop i v =
            case i < end of
              True -> let v' = v // [(i, 0)]
                      in loop (i+k) v'
              False -> v
    in loop (k^2) v

first670 :: U.Vector Int
first670 = let xs = U.fromList [0..5003 :: Int]
       in sieve xs

extend670 :: Int -> U.Vector Int
extend670 n = let f670 = first670
                  backSide = U.generate (n - U.length f670) (\x -> 0 :: Int)
              in (U.++) f670 backSide
