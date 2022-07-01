module Prob26 where
{--

A unit fraction contains 1 in the numerator.  The decimal
representation of the unit fractions with denominators 2 to 10 are
given

Where .1(6) means 0.16666..., and has a 1-digit recurring cycle.  It
can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000, for which 1/d contains the longest
recurring cycle in its decimal fraction part.

Notes:
- No need to do all orders of magnitude.
-- 1/2 covers 1/20, 1/200 (2 orders of magnitude for 2-9)
--- denom < 10, filter 2 orders of mag
-- 1/10 through 1/99 cover 1/100, 1/990 (1 order of magnitude for 10-99)
--- 9 < denom < 100 filter 1 order of mag
-- Trims roughly 100 cases

- How to know when you've hit a cycle...seems kind of like a directed
  graph problem.

- Any better way to compute besides divModding?

- What's an upper bound for the length of repeats?

--}

run26 :: IO ()
run26 = do
  putStr "Problem 26 => "
  putStrLn "unsolved"
