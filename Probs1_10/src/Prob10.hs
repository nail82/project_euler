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
  putStrLn $ show ans

ans :: Int
ans = sumPrimes first670 (MaxPrime 2000000) (CurrentPrime 1) (CurrentSum 1)

newtype MaxPrime =
    MaxPrime Int

newtype CurrentPrime =
    CurrentPrime Int

newtype CurrentSum =
    CurrentSum Int


sumPrimes :: U.Vector Int -> MaxPrime -> CurrentPrime -> CurrentSum -> Int
sumPrimes v (MaxPrime n) (CurrentPrime m) (CurrentSum s) =
          case m >= n of
            True -> s
            False -> let next = findNextPrime (m + 2) v
                         s' = s + m
                     in sumPrimes v (MaxPrime n) (CurrentPrime next) (CurrentSum s')
