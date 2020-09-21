{-# LANGUAGE FlexibleContexts #-}
module VectorTry where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed ((!), (//))
import Data.Word (Word8)

-- https://www.fpcomplete.com/haskell/library/vector/
{-
  Vector Notes:
    Boxed vectors can hold any haskell type (via pointers)
    Unboxed vectors hold actual values, but require instances of the Prim
      type class (ints, chars, doubles, etc).
-}

mutableDoIt :: IO ()
mutableDoIt = do
  mutable <- M.replicate 10 0
  M.write mutable 0 (42 :: Int)
  vector <- U.unsafeFreeze mutable
  putStrLn $ show vector

addInt :: (PrimMonad m, M.MVector v Int)
           => v (PrimState m) Int
               -> Int
               -> Int
               -> m ()
addInt v i w = do
  M.write v i w

unboxedDoIt :: U.Vector Int -> U.Vector Int
unboxedDoIt v =
    let end = U.length v
        loop v i =
            case i < end of
              True -> let v' = v // [(i, i+1)]
                      in loop v' (i+1)
              False -> v
    in loop v 0

main1 :: IO ()
main1 = do
  let v = U.replicate 10 (0 :: Int)
  print $ unboxedDoIt v
