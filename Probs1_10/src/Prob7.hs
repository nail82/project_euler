module Prob7
    (
     run7
    )
    where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!), (//), (++))
import Control.Monad.State

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
  - State is definitely the way to go with this computation.
    Getting the types working and sorting out how to execute
    the state computation was tricky.

  The initial state I need to give the machine is the extend670 n
  I think part of my problem is the random number generator examples
  are clouding my understanding a bit.
-}

run7 :: IO ()
run7 = do
  putStr "Problem 7 => "
  let jv = ans 10001
  putStrLn $ case jv of
               Just v -> show v
               otherwise -> "Something broke"

ans :: Int -> Maybe Int
ans n
    | n < 1     = Just 1
    | n < 671   = Just $ first670 ! (n - 1)
    | otherwise = do let ext = extend670 n
                         n' = n - 670
                     primes <- evalStateT (fillPrimes n') ext
                     case primes of
                       [] -> Nothing
                       ps -> Just (last ps)


fillPrimes :: Int -> StateT (U.Vector Int) Maybe [Int]
fillPrimes n = do
  replicateM n (StateT $ \s -> fillOne s)

fillOne :: U.Vector Int -> Maybe (Int, U.Vector Int)
fillOne s =
    case mIdx of
      Nothing -> Just (U.last s, s)
      Just nextIdx ->
          let lastPrime = s ! (nextIdx - 1)
              nextPrime = findNextPrime (lastPrime+2) s
          in Just (nextPrime, U.update s $ U.fromList [(nextIdx, nextPrime)])
    where mIdx = U.elemIndex 0 s


findNextPrime :: Int -> U.Vector Int -> Int
findNextPrime p v =
    let rootp = croot $ fromIntegral p
        p' = innerloop p 0 rootp v
    in case p' > 0 of
         -- p' is prime
         True -> p'
         -- check the next interger / keep looking
         False -> findNextPrime (p + 1) v
    where
      innerloop p'' k rootp v' =
          case v ! k > rootp of
            -- p is prime
            True -> p''
            False -> case p'' `rem` (v' ! k) == 0 of
                       -- p isn't prime
                       True -> 0
                       -- keep checking if p is prime
                       False -> innerloop p'' (k + 1) rootp v'


-- Generate the first few primes and utility functions
croot :: Double -> Int
croot = ceiling . sqrt

sieve :: U.Vector Int -> U.Vector Int
sieve v =
    let loop i w =
            case i^2 < (U.length v) of
              True -> let v' = sieveHelper i w
                      in loop (i+1) v'
              False -> w
    in U.filter (>1) $ loop 2 v

sieveHelper :: Int -> U.Vector Int -> U.Vector Int
sieveHelper k v =
    let end = U.length v
        loop i w =
            case i < end of
              True -> let v' = w // [(i, 0)]
                      in loop (i+k) v'
              False -> w
    in loop (k^2) v

first670 :: U.Vector Int
first670 = let xs = U.fromList [0..5003 :: Int]
       in sieve xs

extend670 :: Int -> U.Vector Int
extend670 n = let f670 = first670
                  backSide = U.generate (n - U.length f670) (\_ -> 0 :: Int)
              in (U.++) f670 backSide
