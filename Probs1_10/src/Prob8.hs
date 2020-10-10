{-# LANGUAGE QuasiQuotes #-}
module Prob8 where

import Text.RawString.QQ
import Data.Vector.Unboxed as U

{-
  The four adjacent digits in the 1000-digit number that have the
  greatest product are 9 × 9 × 8 × 9 = 5832.

73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450

Find the thirteen adjacent digits in the 1000-digit number that have
the greatest product. What is the value of this product?
-}

run8 :: IO ()
run8 = do
  putStr "Problem 8 => "
  putStrLn $ show ans

ans :: Int
ans = let (_,m) = findMax U.product (0,0) 0 vectorBigNum
      in m


bigNum :: String
bigNum = [r|73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
|]

vectorBigNum :: U.Vector Int
vectorBigNum = U.map toInt $ U.fromList $ Prelude.concat $ lines bigNum

toInt :: Char -> Int
toInt c
      | c == '0' = 0
      | c == '1' = 1
      | c == '2' = 2
      | c == '3' = 3
      | c == '4' = 4
      | c == '5' = 5
      | c == '6' = 6
      | c == '7' = 7
      | c == '8' = 8
      | c == '9' = 9
      | otherwise = 1


findMax :: (U.Vector Int -> Int) -> (Int, Int) -> Int -> U.Vector Int -> (Int, Int)
findMax f (l, m) lhs v =
    case lhs + 13 >= (U.length v) of
      True -> (l, m)
      False -> let m' = max m $ f $ U.slice lhs 13 v
               in case m' > m of
                    True -> findMax f (lhs, m') (lhs + 1) v
                    False -> findMax f (l, m) (lhs + 1) v