module Prob2 where

{-
By considering the terms in the Fibonacci sequence whose values do not
exceed four million, find the sum of the even-valued terms.
-}

run2 :: IO ()
run2 = do
  putStrLn "Problem 2 => "
  putStrLn $ show ans

ans :: Int
ans = undefined

sumHelper :: (Int, Int, Int) -> (Int, Int, Int)
sumHelper (nm2, nm1, s) =
    let c = nm2 + nm1
        s' = case mod c 2 of
               0 -> s+c
               1 -> s
    in (nm1, c, s')
