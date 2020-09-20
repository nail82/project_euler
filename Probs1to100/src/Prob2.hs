module Prob2
    (
     run2
    )
    where

{-
By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms.
-}

run2 :: IO ()
run2 = do
  putStr "Problem 2 => "
  putStrLn $ show $ ans [3..] startState

ans :: [Int] -> (Int, Int, Int) -> Int
ans [] (_, _, s) = s
ans (_:xs) (nm2, nm1, s) =
    case (fromIntegral nm1) > 4.0e6 of
      True -> s
      False -> let t' = sumHelper (nm2, nm1, s)
               in ans xs t'

startState = (1,2,2)

sumHelper :: (Int, Int, Int) -> (Int, Int, Int)
sumHelper (nm2, nm1, s) =
    let c = nm2 + nm1
        s' = case even c of
               True -> s+c
               False -> s
    in (nm1, c, s')
