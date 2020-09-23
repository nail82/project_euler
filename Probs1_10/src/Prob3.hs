module Prob3 where

import qualified GHC.Float as F
import qualified Data.Set as S
import Data.Int
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!), (!?), (//))

{-
  What is the largest prime factor of the number 600851475143?
                                                   8462696833
-}

-- Sieving isn't going to work, bigOne is too big

bigOne :: Int
bigOne = 600851475143

smallOne :: Int
smallOne = 13195

run3 :: IO ()
run3 = do
  let v = sieve $ U.enumFromTo 0 10000
  print $ U.length v

trialDiv :: Int -> Maybe Int
trialDiv a = safeHead [x | x <- [2..a-1], mod a x == 0]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

sieveHelper :: Int -> U.Vector Int -> U.Vector Int
sieveHelper k v =
    let end = U.length v
        loop i v =
            case i < end of
              True -> let v' = v // [(i, 0)]
                      in loop (i+k) v'
              False -> v
    in loop (k^2) v

findNonZero :: Int -> U.Vector Int -> Int
findNonZero i v
    | i > (U.length v)-1 = U.length v
    | v ! i > 0 = i
    | otherwise = findNonZero (i+1) v

sieve :: U.Vector Int -> U.Vector Int
sieve v =
    let loop i v =
            case i^2 < (U.length v) of
              True -> let v' = sieveHelper i v
                      in loop (i+1) v'
              False -> v
    in U.filter (\x -> x > 1) $ loop 2 v

mygcd :: Int -> Int -> Int
mygcd a 0 = a
mygcd a b = gcd b $ a `rem` b

squares = S.fromList [(x^2) :: Int | x <- [2..ceiling $ sqrt (fromIntegral bigOne) + 1]]

fermatFactor :: Int -> Int
fermatFactor n =
    let a = ceiling $ sqrt $ fromIntegral n
    in fermatLoop a n

fermatLoop :: Int -> Int -> Int
fermatLoop a n =
    case S.member b2 squares of
      True -> F.double2Int $ (F.int2Double a) - (sqrt $ fromIntegral b2)
      False -> fermatLoop (a+1) n
    where b2 = a^2 - n

-- a = (ceiling $ sqrt $ fromIntegral bigOne) :: Int
-- b2 = a^2 - bigOne
-- a' = (F.int2Double a) - (sqrt $ fromIntegral b2)
