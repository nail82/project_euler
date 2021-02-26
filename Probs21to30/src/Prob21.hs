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
import Control.Monad.Zip (mzip)

run21 :: IO ()
run21 = let (Just s) = ans21
        in do
          putStr "Problem 21 => "
          putStrLn $ show s

ans21 :: Maybe Int
ans21 = let all_pairs = sequence $ L.nub $ filter (\t -> t /= Nothing) $ fmap aGoodPair [1..10000]
            xs = (fmap . fmap) (\t -> (fst t) + (snd t)) all_pairs
        in fmap sum xs

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

pdvs :: Int -> (Int, Int)
pdvs n = (n, (sum . L.nub . properDivisors) n)

divisorSumList :: [(Int, Int)]
divisorSumList = fmap pdvs [1..10000]

luf :: Maybe Int -> Maybe Int
luf Nothing = Nothing
luf (Just n) = lookup n divisorSumList



checkPair' :: Int -> Maybe (Int, Int)
checkPair' n = let just_n     = Just n
                   transitive = just_n == (luf . luf) just_n
                   just_m     = luf just_n
                   not_same   = just_n /= just_m
               in if (transitive && not_same) then mzip just_n just_m else Nothing

sortPair :: Maybe (Int, Int) -> Maybe (Int, Int)
sortPair Nothing = Nothing
sortPair (Just (m,n)) = if m < n then Just (m,n) else Just (n,m)

aGoodPair :: Int -> Maybe (Int, Int)
aGoodPair = sortPair . checkPair'
