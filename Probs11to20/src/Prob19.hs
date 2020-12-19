module Prob19
    (
     run19
    )
    where
{-
  You are given the following information, but you may prefer to do some
  research for yourself.

  - 1 Jan 1900 was a Monday.
  - Thirty days has September,
  - April, June and November.
  - All the rest have thirty-one,
  - Saving February alone,
  - Which has twenty-eight, rain or shine.
  - And on leap years, twenty-nine.

  - A leap year occurs on any year evenly divisible by 4, but not on a
    century unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth
  century (1 Jan 1901 to 31 Dec 2000)?
-}

import Data.Time

run19 :: IO ()
run19 = do
  putStr "Problem 19 => "
  putStrLn $ show ans19

ans19 :: Int
ans19 = sum $ fmap (sundayIndicator . myAddMonth) [0..1199]

dayOne :: Maybe Day
dayOne = fromGregorianValid 1901 1 1

sundayIndicator :: Maybe Day -> Int
sundayIndicator Nothing = 0
sundayIndicator jd = let jdow = fmap dayOfWeek jd
                     in case jdow of
                          Just Sunday -> 1
                          _ -> 0

addMonth :: Maybe Day -> Integer -> Maybe Day
addMonth Nothing _ = dayOne
addMonth (Just d) n = Just $ addGregorianMonthsClip n d

myAddMonth :: Integer -> Maybe Day
myAddMonth = addMonth dayOne
