module Prob9
    (
     run9
    )
    where

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
  let ja = ans 1000
  case ja of
    Nothing -> putStrLn "Didn't find an answer"
    Just (a,b,c) -> putStrLn $ show (a,b,c) <> " " <> show (a*b*c)

ans :: Int -> Maybe (Int, Int, Int)
ans k = let mn = searchForTrip k
        in case mn of
             [] -> Nothing
             t:_ -> Just $ pythagTriple (fst t) (snd t)


-- Using Euclid's equation, given and m and n, find a,b,c
pythagTriple :: Int -> Int -> (Int, Int, Int)
pythagTriple m n
    | m == n    = (0,0,0) -- degenerate triangle
    | m <  n    = pythagTriple n m
    | otherwise = (a,b,c)
    where a = m*m - n*n
          b = 2*m*n
          c = m*m + n*n

checkIt :: Int -> (Int, Int) -> Bool
checkIt k (m, n)
    | m >= n = (2*m*(m+n) - k) == 0
    | otherwise = checkIt k (n, m)

searchForTrip :: Int -> [(Int, Int)]
searchForTrip k =
    let cartProd = filter (\t -> fst t > snd t) [(m,n)| m <- [2..k], n <- [1..k-1]]
        f = checkIt k
    in filter f cartProd
