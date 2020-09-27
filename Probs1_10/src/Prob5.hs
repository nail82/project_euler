module Prob5
    (
     run5
    ) where

{-
2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all
of the numbers from 1 to 20?
-}

{-
  I went mostly brute force.  The approved solution was a
  combination of finding prime factors and exponents to
  the primes via taking logs.
-}

run5 :: IO ()
run5 = do
  putStr "Problem 5 => "
  putStrLn $ show $ ans 20

ans :: Int -> Int
ans hi =
    let aed = allEvenDiv [2..hi]
        loop n = case aed n of
                   True -> n
                   False -> loop $ n+hi
    in loop hi

allEvenDiv :: [Int] -> Int -> Bool
allEvenDiv xs n =
    and $ fmap (evenDiv n) xs

evenDiv :: Int -> Int -> Bool
evenDiv n d = n `rem` d == 0
