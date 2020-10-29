module Main where

import System.IO
import Newton

main :: IO ()
main = do
  let t = doIt (0, 4.4)
  putStrLn $ show t
