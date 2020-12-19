module Prob15
    (
     run15
    )
    where


{-
  Starting in the top left corner of a 2×2 grid, and only being able
  to move to the right and down, there are exactly 6 routes to the
  bottom right corner.

  How many such routes are there through a 20×20 grid?
-}
{-
  There is a closed form to this as a self-avoiding walk.  It's also
  a binomial coefficient:

(20+20)
(  20 )

The number of combinations (order doesn't matter) is n choose k.
In this case, n = |R| + |D| and k = 20.  |R| is the number of moves
we must make to the right (20) and |D| is the number of moves we
have to make down (also 20).

See here: https://en.wikipedia.org/wiki/Self-avoiding_walk
-}

run15 :: IO ()
run15 = do
  putStr "Problem 15 => "
  putStrLn $ show $ (product [(21 :: Integer)..40]) `div` (product [1..20])
