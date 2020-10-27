module Newton where

{-

Helping Jack with calculus homework.

His problem asks you to find the intersection of f(x) = tan(x) and
g(x) = x using Newton's method.

As a hint, they say let h(x) = tan(x) - x.  What using h(x) does is to
drive the intersection down to the x-axis and from there, we're just
using Newton's method to find the root of h(x).

One problem I see is the asymptotes inherent with tan(x) and trying to
do it on paper with the calculator can be tricky, because it's easy to
overshoot the solution and wind up past an asymptote.

-}

epsilon :: Double
epsilon = 1e-6

f :: Double -> Double
f x = tan x

g :: Double -> Double
g = id

h :: Double -> Double
h x = (f x) - (g x)

h' :: Double -> Double
h' x = ((1 / cos x) ** 2) - 1

y :: Double -> Double
y x = (h x) / (h' x)

xNext :: Double -> Double
xNext x0 = x0 - (y x0)

doIt :: (Int, Double) -> (Int, Double)
doIt (100, x0) = (100, x0)
doIt (i, x0) = let x1 = xNext x0
                   err = abs (x1 - x0)
               in case err <= epsilon of
                  True -> (i, x0)
                  False -> doIt (i+1, x1)

linePairs :: Double -> (Double, Double, Double, Double)
linePairs x0 = let y0 = h x0
                   m  = h' x0
                   (x1, y1) = pointSlope x0 y0 m
               in (x0, x1, y0, y1)

pointSlope :: Double -> Double -> Double -> (Double, Double)
pointSlope x0 y0 m =
    let y1 = y0 + m * (0 - x0)
    in (0, y1)
