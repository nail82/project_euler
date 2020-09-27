module Prob4
    (
     run4
    )
    where


import qualified Data.List as L

{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

run4 :: IO ()
run4 = do
  let palindromes = ans 100 999
  putStr "Problem 4 => "
  putStrLn $ show $ head palindromes

mult :: (Int, Int) -> Int
mult (t1,t2) = t1 * t2

ans :: Int -> Int -> [Int]
ans lo hi =
    let pairs = [(x,y) | x <- [lo..hi], y <- [lo..hi], y >= x]
    in L.reverse $ L.sort $ filter isPalin $ fmap mult pairs

isPalin :: Int -> Bool
isPalin x =
    let xstr = show x
    in xstr == (reverse xstr)
