module Prob14 where
{--
    (
     run14
    )
    where
--}

import qualified Data.IntMap.Strict as M

{-
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
ans14 = let d = collatzData [1..999999]
            m = foldr maxTup (0,0) d
        in fst m


maxTup :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxTup l r = if snd l >= snd r then l else r

collatz :: Int -> [Int]
collatz n = collatz' [n]

collatz' :: [Int] -> [Int]
collatz' [] = []
collatz' (1:xs) = reverse $ 1 : xs
collatz' (x:xs) = let x' = case even x of
                             True -> x `div` 2
                             False -> 3 * x + 1
                  in collatz' $ x' : x : xs

collatz'' :: Int -> Int
collatz'' 1 = 0
collatz'' x = let x' = case even x of
                         True -> x `div` 2
                         False -> 3 * x + 1
              in 1 + collatz'' x'

memCollatz :: Int -> Int
memCollatz = undefined

collatzData :: [Int] -> [(Int, Int)]
collatzData ns = fmap go ns
    where go n = (n, length $ collatz n)

-- A function to take a look at the data
writeData :: IO ()
writeData = do
  let fnm = "collatz.csv"
      d = collatzData [1..999999]
      mylines = L.intercalate "\n" $ fmap show d
  fh <- openFile fnm WriteMode
  hPutStrLn fh mylines
  hClose fh

collatzTable :: M.IntMap Int
collatzTable = let t = collatz 999999
                   alist = mapIt t []
               in M.fromList alist
    where
      mapIt [] ys = ys
      mapIt (1:[])  ys = (1,0) : ys
      mapIt (x:xs) ys = (x, length xs) : mapIt xs ys
