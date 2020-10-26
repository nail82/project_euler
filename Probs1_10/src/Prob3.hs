module Prob3
    (
     run3
    )
    where

import qualified GHC.Float as F
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!), (//))

{-
  What is the largest prime factor of the number 600851475143?
-}

-- Sieving isn't going to work, bigOne is too big

croot = ceiling . sqrt

bigOne :: Int
bigOne = 600851475143

smallOne :: Int
smallOne = 15015

run3 :: IO ()
run3 = do
  let ans = S.findMax $ fermatFactors bigOne
  putStr "Problem 3 => "
  putStrLn $ show ans

-- A table of squares.  Could do this with a call to sqrt
squares :: S.Set Int
squares = S.fromList [ x ^ (2 :: Int) | x <- [0..ceiling $ sqrt (fromIntegral bigOne) + 1]]

fermatFactors :: Int -> S.Set Int
fermatFactors n =
    let t1 = singleFactor n
    in S.fromList $ fst <$> fermatLoop t1 []

fermatLoop :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
fermatLoop t xs
    | snd t == 1 = t : xs
    | otherwise  =    (fermatLoop (singleFactor $ fst t) xs)
                   <> (fermatLoop (singleFactor $ snd t) xs)


singleFactor :: Int -> (Int, Int)
singleFactor 0 = (0,1)
singleFactor 1 = (1,1)
singleFactor 2 = (2,1)
singleFactor n =
    let a = croot $ fromIntegral n
    in go a n
        where
          go a' n' = case S.member b2 squares of
                      True -> let rhs = F.double2Int $ (F.int2Double a') - (sqrt $ fromIntegral b2)
                              in (n' `div` rhs, rhs)
                      False -> go (a'+1) n'
              where b2 = a' ^ (2 :: Int) - n'


-- Fooling around with sieving
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
