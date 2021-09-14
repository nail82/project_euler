module Prob25 where

{--
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
--}

import qualified Data.Vector as V

divMod10 :: (Integral a) => a -> (a,a)
divMod10 = flip divMod 10

-- Recursively add a vector of tuples representing two big integers.
-- Note: Most significant digit is in the zeroth postition (big endian).
addTups :: (Integral a) => V.Vector (a,a) -> a -> V.Vector a -> V.Vector a
addTups ts c res
    | V.null ts =
      if c == 0 then res else V.cons c res
    | otherwise =
        let (t,ts') = (V.last ts, V.init ts)
            (carry,placeValue) = divMod10 $ (fst t) + (snd t) + c
            res' = V.cons placeValue res
        in addTups ts' carry res'

-- Use addTups to add two big integers.
bigNumberAdder :: (Integral a) => V.Vector a -> V.Vector a -> V.Vector a
bigNumberAdder lhs rhs =
    -- length rhs will always be >= length lhs, since fibonacci is monotonic
    let pad = V.replicate (V.length rhs - V.length lhs) 0
        lhs' = pad <> lhs
        zipped = V.zip lhs' rhs
    in addTups zipped 0 V.empty

-- Search for a fibonacci of a certain length.
findFibOfLenN :: Int -> (Int, V.Vector Int)
findFibOfLenN n =
    let go k i lhs rhs =

            let rhs' = bigNumberAdder lhs rhs
                i' = i + 1
            in if V.length rhs' >= n then (i',rhs') else go k i' rhs rhs'

    in go n 2 (V.fromList [1]) (V.fromList [1])

ans25 :: String
ans25 =
    let (ans,_) = findFibOfLenN 1000
    in show ans

run25 :: IO ()
run25 = do
  putStr "Problem 25 => "
  putStrLn ans25
