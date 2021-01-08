module Prob21 where

{-
  Let d(n) be defined as the sum of proper divisors of n (numbers less
  than n which divide evenly into n).  If d(a) = b and d(b) = a, where a
  ≠ b, then a and b are an amicable pair and each of a and b are called
  amicable numbers.

  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
  22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284
  are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.
-}

import qualified Data.List as L

run21 :: IO ()
run21 = do
  putStr "Problem 21 => "
  putStrLn "unsolved"

croot :: Int -> Int
croot = ceiling . sqrt . fromIntegral


checkDivisor :: Int -> Int -> (Int, Int)
checkDivisor n d = let t = n `divMod` d
                   in case snd t of
                        0 -> (d,fst t)
                        _ -> (d, -1)

properDivisors :: Int -> [Int]
properDivisors n = let rhs = croot n
                       ds = [2..rhs]
                       ts = fmap (checkDivisor n) ds
                       ws = filter (\t -> snd t /= (-1)) ts
                   in L.concat $ [1] : fmap (\t -> [fst t, snd t]) ws

d :: Int -> (Int, Int)
d n = (n, (sum . L.nub . properDivisors) n)

amicableAlist = fmap d [1..1000]

checkPair :: Int -> Bool
checkPair n = let ji = lookup n amicableAlist
                  rhs = case ji of
                          Just m -> lookup m amicableAlist
                          Nothing -> Nothing
              in (Just n) == rhs
