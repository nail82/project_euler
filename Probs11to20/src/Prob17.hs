module Prob17 where

{-
  If the numbers 1 to 5 are written out in words: one, two, three, four,
  five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were
  written out in words, how many letters would be used?


  NOTE: Do not count spaces or hyphens. For example, 342 (three
  hundred and forty-two) contains 23 letters and 115 (one hundred and
  fifteen) contains 20 letters. The use of "and" when writing out
  numbers is in compliance with British usage.
-}

import Text.Trifecta

run17 :: IO ()
run17 = do
  putStr "Problem 17 => "
  putStrLn "not solved"

data PlaceValue =
    Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    | Fourteen
    | Fifteen
    | Sixteen
    | Seventeen
    | Eighteen
    | Nineteen
    | Twenty
    | Thirty
    | Forty
    | Fifty
    | Sixty
    | Seventy
    | Eighty
    | Ninety
    | Hundred
    deriving (Eq, Show)

newtype Ones      = Ones PlaceValue deriving (Eq, Show)
newtype Tens      = Tens PlaceValue deriving (Eq, Show)
newtype Hundreds  = Hundreds PlaceValue deriving (Eq, Show)

data MyNumber = MyNumber PlaceValue PlaceValue PlaceValue deriving (Eq, Show)
