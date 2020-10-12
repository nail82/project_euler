module Prob10 where

import qualified Data.Vector.Unboxed as U
import Prob7

{-
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
-}

run10 :: IO ()
run10 = do
  putStr "Problem 10 => "
  putStrLn "not solved"

ans :: Int
ans = undefined

-- Kick this off with first670 2000000 5003 $ U.sum first670
sumPrimes :: U.Vector Int -> Int -> Int -> Int -> Int
sumPrimes v n m s =
          case m >= n of
            True -> s
            False -> let next = findNextPrime (m + 2) v
                     in sumPrimes v n next (s+m)
