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

{-
  Reading learn u a haskell about the state monad
  - State is definitely the way to go with this computation.  I see glimmers
    of how to do it, but it's still trying to coalesce in my head.

  - Here's what I'm thinking for types
  findNext :: U.Vector -> (Int, U.Vector)
  wrapFind :: State findNext

  - Finding the nth prime will look something like:
  runState nthPrime $ extend670 n

  The initial state I need to give the machine is the extend670 n
  I think part of my problem is the random number generator examples
  are clouding my understanding a bit.
-}

run7 :: IO ()
run7 = do
  putStr "Problem 7 => "
  putStrLn "unsolved"


ans :: Int
ans = undefined

croot :: Double -> Int
croot = ceiling . sqrt

nthHelper = undefined

findNext :: U.Vector Int -> (Int, U.Vector Int)
findNext s =
    let nextIdx = U.elemIndex 0 s
        lastPrime = s ! (nextIdx - 1)
        nextPrime = findNextPrime lastPrime s
    in (nextPrime, U.update s $ U.fromList [(nextIdx, nextPrime)])



findNextPrime :: Int -> U.Vector Int -> Int
findNextPrime p v =
    let stop = croot $ fromIntegral p
        k = 0
        p' = innerloop p k stop v
    in case p' > 0 of
         -- p' is prime
         True -> p'
         -- check the next interger / keep looking
         False -> findNextPrime (p + 1) v
    where
      innerloop p k stop v =
          case v ! k > stop of
            -- p is prime
            True -> p
            False -> case p `rem` (v ! k) == 0 of
                       -- p isn't prime
                       True -> 0
                       -- keep checking if p is prime
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

myStateF :: Int -> (Int, Int)
myStateF x = (x+2, x+3)
