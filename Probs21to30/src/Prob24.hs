{-# LANGUAGE NumericUnderscores #-}
module Prob24 where
{-
A permutation is an ordered arrangement of objects. For example, 3124
is one possible permutation of the digits 1, 2, 3 and 4. If all of the
permutations are listed numerically or alphabetically, we call it
lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2,
3, 4, 5, 6, 7, 8 and 9?
-}

import qualified Data.List as L
import qualified Data.Vector as V
import Data.Vector ((//), (!))

-- Recursive solution
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

showAns :: [Int] -> String
showAns = foldr (\x s -> show x <> s) ""

ans24 :: String
ans24 =
    let xs = [0..9] :: [Int]
        n = L.product [1..length xs]
        perms = permuteList xs
        ans = snd $ head $ L.dropWhile (\t -> fst t < 1_000_000) (zip [1..n] perms)
     in showAns ans



-- End recursive solution

-- Linear solution
{--
Here is a linear solution from Knuth.
Assumes a sorted list `a' with n sorted elements indexed from 1 to n.
Also assumes an element at index 0 that is strictly less than a_n.
This will produce permutations in lexicographic order.

let j = n-1

while a_j >= a_j+1
  j <- j-1 -- stop if j=0

let l = n
while a_j >= a_l
  l <- l-1

swap a_j a_l

l <- n
let k = j+1

while (k < l)
  swap a_k a_l
  k <- k+1
  l <- l-1

-- repeat

--}

findJ :: V.Vector Int -> (Int, V.Vector Int)
findJ v =
    let n = V.length v - 1
        go 0 = 0
        go j'
            | (v ! j') < (v ! (j'+1)) = j'
            | otherwise = go (j'-1)

        j = go (n-1)
    in (j,v)

findL :: (Int, V.Vector Int) -> (Int, V.Vector Int)
findL (0,v) = (0,v)
findL (j,v) =
    let l = V.length v - 1

        go 0 = 0
        go l'
           | (v ! j) < (v ! l') = l'
           | otherwise = go (l'-1)

        l'' = go l
        v' = swapEm v (j,l'')

    in (j,v')

swapEm :: V.Vector Int -> (Int, Int) -> V.Vector Int
swapEm v (i1,i2) =
    let a1 = v ! i1
        a2 = v ! i2
    in v // [(i1,a2), (i2,a1)]

moveK :: (Int, V.Vector Int) -> V.Vector Int
moveK (0,v) = v
moveK (j,v) =
    let k = j+1
        l = V.length v - 1

        go v' k' l'
           | (k' >= l') = v'
           | otherwise =
               let v'' = swapEm v' (k',l')
                   k'' = k'+1
                   l'' = l'-1
               in go v'' k'' l''

    in go v k l

onePermute :: V.Vector Int -> V.Vector Int
onePermute = moveK . findL . findJ

statePermute :: (Int, V.Vector Int) -> V.Vector Int
statePermute (1_000_000,v) = v
statePermute (i,v) =
    let v' = onePermute v
    in statePermute (i+1, v')

ans24' :: String
ans24' =
    let v = V.fromList [-1..9]
        (_:ans) = V.toList $ statePermute (1,v)
    in showAns ans

-- End linear solution

run24 :: IO ()
run24 = do
  putStr "Problem 24 => "
  putStrLn ans24
