{-# LANGUAGE NumericUnderscores #-}
module Prob24 where



import qualified Data.List as L


-- Insert an element at the ends and between elements of a list
insertElem :: a -> [a] -> [[a]]
insertElem y ys =
    let recInter s x xs zs =
            let (lhs, rhs) = L.splitAt s xs
                zs' = (lhs <> [x] <> rhs) : zs
            in case rhs of
                 [] -> zs'
                 _ -> recInter (s+1) x xs zs'

    in recInter 0 y ys []

-- Permute a list and return it sorted
permuteList' :: (Ord a) => [a] -> [[a]] -> [[a]]
permuteList' [] xxs = L.sort xxs
permuteList' (x:xs) xxs =
    let xxs' = xxs >>= insertElem x
    in permuteList' xs xxs'

permuteList :: (Ord a) => [a] -> [[a]]
permuteList xs = permuteList' xs [[]]

ans24 :: String
ans24 =
    let showAns = foldr (\x s -> show x <> s) ""
        xs = [0..9] :: [Int]
        n = L.product [1..length xs]
        perms = permuteList xs
        ans = snd $ head $ L.dropWhile (\t -> fst t < 1_000_000) (zip [1..n] perms)
     in showAns ans

run24 :: IO ()
run24 = do
  putStr "Problem 24 => "
  putStrLn ans24
