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

showAns :: (Traversable t, Show a) => t a -> String
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

-- step 1
let j = n-1
while a_j >= a_j+1
  j <- j-1 -- stop if j=0

-- step 2
let l = n
while a_j >= a_l
  l <- l-1
swap a_j a_l

-- step 3
l <- n
let k = j+1
while (k < l)
  swap a_k a_l
  k <- k+1
  l <- l-1

Repeat until we hit the permutation we want.

I had two key insights converting Knuth's algorithm to a functional
form.

The first was realizing the j index and input vector flow through all
three steps.  Secondly, once we hit the last permutation (reverse
ordering), subsequent iterations can continue to return this last
permutation.  In this way, I didn't have to make any allowance for
bailing out at step 1 as Knuth's algorithm suggests.

--}

swapEm :: (Ord a) => V.Vector a -> (Int, Int) -> V.Vector a
swapEm v (i1,i2) =
    let a1 = v ! i1
        a2 = v ! i2
    in v // [(i1,a2), (i2,a1)]

-- Find j
step1 :: (Ord a) => V.Vector a -> (Int, V.Vector a)
step1 v =
    let n = V.length v - 1
        go 0 = 0
        go j'
            | (v ! j') < (v ! (j'+1)) = j'
            | otherwise = go (j'-1)

        j = go (n-1)
    in (j,v)

-- Find l, swap a_j & a_l
step2 :: (Ord a) => (Int, V.Vector a) -> (Int, V.Vector a)
step2 (0,v) = (0,v)
step2 (j,v) =
    let go 0 = 0
        go l
           | (v ! j) < (v ! l) = l
           | otherwise = go (l-1)

        l' = go $ V.length v - 1
        v' = swapEm v (j,l')

    in (j,v')

-- Reverse the back of the list starting at a_j+1.
-- Step 3 only executes when a_j+1 .. a_n are in
-- reverse order, meaning we've visted all permutations
-- with prefix a_1 .. a_j.
step3 :: (Ord a) => (Int, V.Vector a) -> V.Vector a
step3 (0,v) = v
step3 (j,v) =
    let go v' k l
            | (k >= l) = v'
            | otherwise =
                let v'' = swapEm v' (k,l)
                    k' = k+1
                    l' = l-1
                in go v'' k' l'

    in go v (j+1) (V.length v - 1)

oneIteration :: (Ord a) => V.Vector a -> V.Vector a
oneIteration = step3 . step2 . step1

nPermutations :: (Ord a) => Int -> V.Vector a -> V.Vector a
nPermutations n v =
    let go :: (Ord a) => (Int, V.Vector a) -> V.Vector a
        go (i,v')
            | i == n = v'
            | otherwise = go (i+1, oneIteration v')
    in go (1,v)

ans24' :: String
ans24' =
    let xs = V.fromList [-1..9] :: V.Vector Int
        v' = nPermutations 1_000_000 xs
    in showAns $ V.tail v'

-- Linear solution is a 2x speed up.
-- End linear solution

run24 :: IO ()
run24 = do
  putStr "Problem 24 => "
  putStrLn ans24'
