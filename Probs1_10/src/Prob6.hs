module Prob6
    (
     run6
    )
    where

{-
  The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + .. 10^2 = 385

  The square of the sum of the first ten natural numbers is,
    (1 + 2 + 3 .. 10)^2 = 55^2 = 3025

  Hence the difference between the sum of the squares of the first ten
  natural numbers and the square of the sum is 3025 - 385 = 2640

  Find the difference between the sum of the squares of the first one
  hundred natural numbers and the square of the sum.
-}

run6 :: IO ()
run6 = do
  putStr "Problem 6 => "
  putStrLn $ show ans

ans :: Int
ans = let lhs = (sumOfN 100) ^ (2 :: Int)
          rhs = sumEm (fmap (flip (^) (2 :: Int)) [1..100])
      in lhs - rhs

sumEm :: [Int] -> Int
sumEm xs = foldr (+) 0 xs

sumOfN :: Int -> Int
sumOfN n = n * (n+1) `div` 2
