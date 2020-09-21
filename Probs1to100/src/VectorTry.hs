module VectorTry where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- https://www.fpcomplete.com/haskell/library/vector/

buildOne :: Int -> UM.MVector
buildOne n = UM.new n
