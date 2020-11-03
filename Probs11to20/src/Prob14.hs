module Prob14
    (
     run14
    )
    where

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
