module Prob9 where

{-
A Pythagorean triplet is a set of three natural numbers, a < b < c,
for which, a^2 + b^2 = c^2

There is one triple where a + b + c = 1000.

Find it.

sqrt(a*a + b*b) + a + b = 1000
-}

run9 :: IO ()
run9 = do
  putStr "Problem 9 => "
  putStrLn "not solved"

trySomething :: Double -> Double -> Double
trySomething a b = sqrt (a*a + b*b) + a + b

-- Using Euclid's equation, given and m and n, find a,b,c
pythagTriple :: Int -> Int -> (Int, Int, Int)
pythagTriple m n = (a,b,c)
    where a = m*m - n*n
          b = 2*m*n
          c = m*m + n*n

isAlmostInt :: Float -> Bool
isAlmostInt x = (abs $ x - (fromIntegral $ round x)) < err
    where err = 1e-10

isAlmostZero :: Double -> Bool
isAlmostZero x = (abs x) < err
    where err = 1e-10
