module Prob28 where

{--

Starting with the number 1 and moving to the right in a clockwise
direction a 5 by 5 spiral is formed as follows:

  21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is
101.

What is the sum of the numbers on the diagonals in a 1001 by 1001
spiral formed in the same way.

Notes:
Dimensions are the odds as the square grows
Let the dimension be n
Upper right (ur) = n^2
Upper left  (ul) = ur - (n-1)
Lower left  (ll) = ul - (n-1)
Lower right (lr) = ll - (n-1)

Sum of the corners is
  n^2 - ( k (n-1) )
where n = dimension of the square
and k = [v | v <- [1..3]]
numbers the corners from top left to bottom right
in counter-clockwise order.

7  8  9
6  1  2
5  4  3

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

43 44 45 46 47 48 49
42 21 22 23 24 25 26
41 20  7  8  9 10 27
40 19  6  1  2 11 28
39 18  5  4  3 12 29
38 17 16 15 14 13 30
37 36 35 34 33 32 31

--}

-- Closed solution to corner sum
sumCorners :: Integer -> Integer
sumCorners n =
    let ks = [1..3]
        n2 = n ^ 2
        f k = n2 - k * n + k
    in sum $ n2 : [f k | k <- ks]

run28 :: IO ()
run28 = do

  let ns = [n | n <- [3..1001], odd n]
      ans = 1 + sum (fmap sumCorners ns)

  putStr "Problem 28 => "
  print ans
