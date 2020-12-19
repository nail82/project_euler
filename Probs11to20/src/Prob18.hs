{-# LANGUAGE QuasiQuotes #-}
module Prob18
    (
     run18
    )
    where

import Text.RawString.QQ
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Matrix ((!))
import System.IO
import Control.Monad.State

{-
  By starting at the top of the triangle below and moving to adjacent
  numbers on the row below, the maximum total from top to bottom is
  23.

  3
  7 4
  2 4 6
  8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom of the triangle below:

  75
  95 64
  17 47 82
  18 35 87 10
  20 04 82 47 65
  19 01 23 75 03 34
  88 02 77 73 07 63 67
  99 65 04 28 06 16 70 92
  41 41 26 56 83 40 80 70 33
  41 48 72 33 47 32 37 16 94 29
  53 71 44 65 25 43 91 52 97 51 14
  70 11 33 28 77 73 17 78 39 68 17 57
  91 71 52 38 17 14 91 43 58 50 27 29 48
  63 66 04 68 89 53 67 30 73 16 69 87 40 31
  04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

-}

treeData :: String
treeData = [r|75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
|]

smallTree = [r|3
7 4
2 4 6
8 5 9 3
|]

run18 :: IO ()
run18 = do
  putStr "Problem 18 => "
  putStrLn $ show $ ans18
  ans67

ans18 :: Int
ans18 = (reduceTree . makeTree) treeData


data Tree a = Leaf
            | Node (Tree a) a (Tree a)
              deriving (Eq, Show)

reduceTree :: Tree Int -> Int
reduceTree Leaf = 0
reduceTree (Node left n right) = n + max (reduceTree left) (reduceTree right)

countNodes :: Tree Int -> Int
countNodes Leaf = 0
countNodes (Node left _ right) = 1 + (countNodes left) + (countNodes right)


makeTreeGrid :: String -> M.Matrix Int
makeTreeGrid s =
    let rs = lines s
        cols = length rs
        ms = fmap (padRow cols) rs
    in M.fromLists ms

padRow :: Int -> String -> [Int]
padRow cols s = let triCols = (fmap (\x -> read x :: Int) $ words s)
                    pad = cols - (length triCols)
                in triCols <> (take pad $ repeat 0)

makeTree :: String -> Tree Int
makeTree s = let mat = makeTreeGrid s
                 dim = M.nrows mat
             in treeHelper dim (1,1) mat

-- Assumes a square matrix
treeHelper :: Int -> (Int, Int) -> M.Matrix Int -> Tree Int
treeHelper dim (rs,cs) mat =
    case rs+1 > dim of
      True -> Node Leaf (mat ! (rs,cs)) Leaf
      False -> Node
               (treeHelper dim (rs+1, cs) mat)
               (mat ! (rs,cs))
               (treeHelper dim (rs+1, cs+1) mat)

-- Recursive solution no workie for 67.
-- Here's a dynamic programming solution.
fnm :: String
fnm = "/Users/tsatcher/Downloads/p067_triangle.txt"

ans67 :: IO ()
ans67 = do
  putStr "Problem 67 => "
  fh <- openFile fnm ReadMode
  strdata <- hGetContents fh
  let tg = makeTreeGrid strdata
      jn = reduceTreeDP tg
  case jn of
    Just n -> putStrLn $ show n
    Nothing -> putStrLn "something broke"

reduceTreeDP :: M.Matrix Int -> Maybe Int
reduceTreeDP mat = do
    let rows = M.nrows mat - 1
        jvecs = evalStateT (matrixReduce rows) (rows, mat)
    case jvecs of
      Just vecs -> case vecs of
                     [] -> Nothing
                     vs -> Just $ (last vs) V.! 0
      Nothing -> Nothing

matrixReduce :: Int -> StateT (Int, M.Matrix Int) Maybe [V.Vector Int]
matrixReduce n = do
  replicateM n (StateT $ \t -> reduceRow' t)

reduceRow' :: (Int, M.Matrix Int) -> Maybe (V.Vector Int, (Int, M.Matrix Int))
reduceRow' (row, mat) =
    let tups = (\x -> (row,x)) <$> [1..row]
        mat' = foldr reduceElem mat tups
    in Just (M.getRow row mat', (row-1, mat'))

reduceElem :: (Int, Int) -> M.Matrix Int -> M.Matrix Int
reduceElem (row, col) mat =
    let v = max (mat ! (row+1, col)) (mat ! (row+1, col+1))
        w = mat ! (row, col)
    in M.setElem (v+w) (row, col) mat
