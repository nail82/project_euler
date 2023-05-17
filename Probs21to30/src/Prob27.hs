module Prob27 where

{--

Euler discovered the remarkable quadratic formula:

  n^{2} + n + 41

It turns out that the formula will produce 40 primes for the
consecutive integer values 0 <= n < 39. However, when

  n = 40,
  40^{2} + 40 + 41 = 40(40 + 1) + 41

is divisible by 41, and certainly when

  n = 41, 41^{2} + 41 + 41

is clearly divisible by 41.

The incredible formula

  n^{2} - 79n + 1601

was discovered, which produces 80 primes for the consecutive values

  0 <= n <= 79.

The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

  n^{2} + an + b,

where  |a| < 1000 and |b| <= 1000.

Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with 0.

Notes:
b has to be positive and prime, since it is the solution at n = 0
b^2 + b(1) + b isn't prime

168 primes <= 1000

Naive approach
foreach n
  foreach a
    foreach b

The primes produced are NOT unique

--}

-- Build some intuition
eulerEq n =
    n^2 + n + 41

euler' n =
    n^2 - 79 * n + 1601

run27 :: IO ()
run27 = do
  putStr "Problem 27 => "
  putStrLn "unsolved"
