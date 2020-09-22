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

unboxedDoIt :: Int -> U.Vector Int -> U.Vector Int
unboxedDoIt s v =
    let end = U.length v
        loop i v =
            case i < end of
              True -> let v' = v // [(i, 0)]
                      in loop (i+s) v'
              False -> v
    in loop s v

main1 :: IO ()
main1 = do
  let v = U.enumFromTo 0 10
  print $ unboxedDoIt 2 v
