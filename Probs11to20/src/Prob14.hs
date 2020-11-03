module Prob14 where

run14 :: IO ()
run14 = do
  putStr "Problem 14 => "
  putStrLn "not solved"

collatz :: [Int] -> [Int]
collatz [] = []
collatz (1:xs) = reverse $ 1 : xs
collatz (x:xs) = let x' = case even x of
                            True -> x `div` 2
                            False -> 3 * x + 1
                 in collatz $ x' : x : xs

collatzData :: [Int] -> [(Int, Int)]
collatzData ns = fmap go ns
    where go n = (n, length $ collatz [n])
