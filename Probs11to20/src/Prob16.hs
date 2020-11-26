module Prob16 where

{-
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

  What is the sum of the digits of the number 2^1000?
-}

run16 :: IO ()
run16 = do
  putStr "Problem 16 => "
  putStrLn $ show ans16

toInt :: Char -> Int
toInt s = case s of
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

bigOne :: Integer
bigOne = 2^1000

ans16 :: Int
ans16 = sum $ fmap toInt $ show bigOne
