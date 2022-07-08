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

--}

import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.List as L

computeOnePlace :: Integer -> Integer -> (Integer, Integer)
computeOnePlace d n =
    let n' = n * 10
        place = (n' `div` d)
        rhs =  place * d
    in (n' - rhs, place)

oneHelper :: S.Set Integer
   -> V.Vector Integer
   -> (Integer -> (Integer, Integer))
   -> Integer
   -> V.Vector Integer
oneHelper ns places f n =
    let (n',place) = f n
        g q
            | q `S.member` ns = places
            | q == 0 = places `V.snoc` place
            | otherwise = let ns' = S.insert q ns
                              places' = places `V.snoc` place
                          in oneHelper ns' places' f q
    in g n'


oneOver :: Integer -> V.Vector Integer
oneOver d =
    let places = oneHelper S.empty V.empty (computeOnePlace d) 1
        trimmed = V.dropWhile (== 0) places
    in if V.length places == V.length trimmed then
           places
       else
           trimmed `V.snoc` 0

foldOne :: Integer -> (Integer, Integer) -> (Integer, Integer)
foldOne n' (p,m) =
    let m' = fromIntegral $ V.length $ oneOver n'
    in if m' > m then
           (n',m')
       else
           (p,m)


ans :: Integer
ans =
    let (n,_) = L.foldr foldOne (1,0) [2..999]
    in n


run26 :: IO ()
run26 = do
  putStr "Problem 26 => "
  print ans
