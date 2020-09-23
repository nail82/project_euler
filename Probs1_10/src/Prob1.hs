module Prob1
    (
     run1
    )
    where

{-
  Find the sum of the integers between [1,999]
  that are factors of 3 or 5
-}

run1 :: IO ()
run1 = do
  putStr "Problem 1 => "
  putStrLn $ show ans

filt :: Int -> Bool
filt x = (mod x 3 == 0) || (mod x 5 == 0)

ans :: Int
ans = sum $ filter filt [1..999]
