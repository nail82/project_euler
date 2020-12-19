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

import qualified Data.List as L

run17 :: IO ()
run17 = do
  putStr "Problem 17 => "
  putStrLn $ show ans17

ans17 :: Int
ans17 = length $ L.intercalate "" $ "onethousand" : fmap (show . buildMyNumber) [1..999]

data MyNumber = MyNumber Hundreds Tens Ones deriving (Eq)

data Ones =
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
    deriving (Eq)

data Tens =
    Naught
    | Ten
    | Twenty
    | Thirty
    | Forty
    | Fifty
    | Sixty
    | Seventy
    | Eighty
    | Ninety
    deriving (Eq)

data Hundreds =
    NaughtHundred
    | OneHundred
    | TwoHundred
    | ThreeHundred
    | FourHundred
    | FiveHundred
    | SixHundred
    | SevenHundred
    | EightHundred
    | NineHundred
      deriving (Eq)

instance Show Ones where
    show Zero  = ""
    show One   = "one"
    show Two   = "two"
    show Three = "three"
    show Four  = "four"
    show Five  = "five"
    show Six   = "six"
    show Seven = "seven"
    show Eight = "eight"
    show Nine  = "nine"

instance Show Tens where
    show Naught  = ""
    show Ten     = "ten"
    show Twenty  = "twenty"
    show Thirty  = "thirty"
    show Forty   = "forty"
    show Fifty   = "fifty"
    show Sixty   = "sixty"
    show Seventy = "seventy"
    show Eighty  = "eighty"
    show Ninety  = "ninety"

instance Show Hundreds where
    show NaughtHundred = ""
    show OneHundred    = "onehundred"
    show TwoHundred    = "twohundred"
    show ThreeHundred  = "threehundred"
    show FourHundred   = "fourhundred"
    show FiveHundred   = "fivehundred"
    show SixHundred    = "sixhundred"
    show SevenHundred  = "sevenhundred"
    show EightHundred  = "eighthundred"
    show NineHundred   = "ninehundred"

tenShow :: Ones -> String
tenShow Zero  = "ten"
tenShow One   = "eleven"
tenShow Two   = "twelve"
tenShow Three = "thirteen"
tenShow Four  = "fourteen"
tenShow Five  = "fifteen"
tenShow Six   = "sixteen"
tenShow Seven = "seventeen"
tenShow Eight = "eighteen"
tenShow Nine  = "nineteen"

instance Show MyNumber where
    show (MyNumber NaughtHundred Ten o) = tenShow o
    show (MyNumber NaughtHundred t o) = (show t) <> (show o)
    show (MyNumber h Naught Zero) = show h
    show (MyNumber h Ten o) = (show h) <> "and" <> (tenShow o)
    show (MyNumber h t o) = (show h) <> "and" <> (show t) <> (show o)

convertOnes :: Int -> Ones
convertOnes 0 = Zero
convertOnes 1 = One
convertOnes 2 = Two
convertOnes 3 = Three
convertOnes 4 = Four
convertOnes 5 = Five
convertOnes 6 = Six
convertOnes 7 = Seven
convertOnes 8 = Eight
convertOnes 9 = Nine
convertOnes _ = Zero

convertTens :: Int -> Tens
convertTens 0 = Naught
convertTens 1 = Ten
convertTens 2 = Twenty
convertTens 3 = Thirty
convertTens 4 = Forty
convertTens 5 = Fifty
convertTens 6 = Sixty
convertTens 7 = Seventy
convertTens 8 = Eighty
convertTens 9 = Ninety
convertTens _ = Naught

convertHundreds :: Int -> Hundreds
convertHundreds 0 = NaughtHundred
convertHundreds 1 = OneHundred
convertHundreds 2 = TwoHundred
convertHundreds 3 = ThreeHundred
convertHundreds 4 = FourHundred
convertHundreds 5 = FiveHundred
convertHundreds 6 = SixHundred
convertHundreds 7 = SevenHundred
convertHundreds 8 = EightHundred
convertHundreds 9 = NineHundred
convertHundreds _ = NaughtHundred

findPlaceValues :: Int -> Int -> [Int] -> [Int]
findPlaceValues 0 _ ns = ns
findPlaceValues dv n ns =
    let t = n `divMod` dv
    in (fst t) : findPlaceValues (dv `div` 10) (snd t) ns



buildMyNumber :: Int -> MyNumber
buildMyNumber n
    | n > 999 || n < 1 = MyNumber NaughtHundred Naught Zero
    | otherwise = let places = findPlaceValues 100 n []
                  in MyNumber
                         (convertHundreds $ places !! 0)
                         (convertTens $ places !! 1)
                         (convertOnes $ places !! 2)
