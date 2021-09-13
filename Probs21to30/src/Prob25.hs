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
import Data.Vector ((!))

divMod10 = flip divMod 10

addTups :: V.Vector (Int,Int) -> Int -> V.Vector Int -> V.Vector Int
addTups ts c res
    | V.null ts =
      if c == 0 then res else V.cons c res
    | otherwise =
        let (t,ts') = (V.head ts, V.tail ts)
            (carry,placeValue) = divMod10 $ (fst t) + (snd t) + c
        in addTups ts' carry (V.cons placeValue res)


bigNumberAdder :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
bigNumberAdder = undefined

run25 :: IO ()
run25 = do
  putStr "Problem 25 => "
  putStrLn "unsolved"
