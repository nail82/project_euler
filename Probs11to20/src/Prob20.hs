module Prob20 where

{-
  n! means n × (n − 1) × ... × 3 × 2 × 1

  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800, and the sum
  of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!
-}

run20 :: IO ()
run20 = do
  putStr "Problem 20 => "
  putStrLn $ show ans20


ans20 :: Integer
ans20 = sum $ fmap toInt $ show $ product [(1 :: Integer)..100]

toInt :: Char -> Integer
toInt c =
    case c of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      '3' -> 3
      '4' -> 4
      '5' -> 5
      '6' -> 6
      '7' -> 7
      '8' -> 8
      '9' -> 9
      _ -> 0
