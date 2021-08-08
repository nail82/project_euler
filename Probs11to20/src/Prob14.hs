module Prob14
    (
     run14
    )
    where

import qualified Data.IntMap.Strict as M

{-

The following iterative sequence is defined for the set of positive
integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following
sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz
Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million


  Reading the approved solution, it is better to memoize collatz
  vice computing each chain.

  See here:  wiki.haskell.org/Memoization for some ideas.

  Memoizing is trickier because collatz n+1 doesn't necessarily
  mean we have already visited collatz n, n-1, .. 1.

  Also, there are some heuristics that can be applied to speed
  things along.  One is that collatz n = 1 + collatz n/2, when n is even.

  Better just to compute the sum vice creating the whole chain.
-}

import System.IO
import qualified Data.List as L

run14 :: IO ()
run14 = do
  putStr "Problem 14 => "
  putStrLn $ show ans14

ans14 :: Int
ans14 = snd $ M.findMax $ makeCollatzMap [500000..999999] $ M.singleton 0 1


makeCollatzMap :: [Int] -> M.IntMap Int -> M.IntMap Int
makeCollatzMap [] m = m
makeCollatzMap (x:xs) m = let t = collatzTup x
                              m' = M.insertWith max (fst t) (snd t) m
                          in makeCollatzMap xs m'

collatzTup :: Int -> (Int, Int)
collatzTup n = (collatz'' n, n)

invCollatzTup :: Int -> (Int, Int)
invCollatzTup n = let t = collatzTup n
                  in (snd t, fst t)

collatz'' :: Int -> Int
collatz'' 1 = 0
collatz'' x = let x' = case even x of
                         True -> x `div` 2
                         False -> 3 * x + 1
              in 1 + collatz'' x'


collatzData :: [Int] -> [(Int, Int)]
collatzData = fmap invCollatzTup

-- A function to take a look at the data
writeData :: IO ()
writeData = do
  let fnm = "collatz.csv"
      d = collatzData [1..999999]
      mylines = L.intercalate "\n" $ fmap show d
  fh <- openFile fnm WriteMode
  hPutStrLn fh mylines
  hClose fh
