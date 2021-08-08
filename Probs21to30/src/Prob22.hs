{-# LANGUAGE QuasiQuotes #-}
module Prob22
    (
     run22
    ) where

{--

Using names.txt a text file containing over five-thousand first names,
begin by sorting it into alphabetical order. Then working out the
alphabetical value for each name, multiply this value by its
alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN,
which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the
list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?

--}

import System.IO
import Text.RawString.QQ
import qualified System.Directory as SD
import qualified Data.List as L
import Data.Char (ord)

run22 :: IO ()
run22 = do
  fnm <- SD.makeAbsolute "../resources/p022_names.txt"
  fh <- openFile fnm ReadMode
  strdata <- hGetContents fh
  let names = splitInput' strdata
  let ans = ans22 names
  putStr "Problem 22 => "
  putStrLn $ show ans

{-- Monad sequencing finally makes sense.
run22' :: IO ()
run22' = SD.makeAbsolute "../resources/p022_names.txt"
         >>= (flip openFile) ReadMode
         >>= hGetContents
         >>= putStrLn
--}

foldNames (nm,idx) z = z + (wordVal nm) * idx
ans22 names =
    let zipped = zip names [1..(length names)]
    in foldr foldNames 0 zipped

fakeNames :: String
fakeNames = [r|"JOE","BUB","JOHN"|]

-- Take the input string apart by hand.  Just because.
readName :: (String, String) -> (String, String)
readName (name, "") = (reverse name, "")
readName (name,s:ss)
    | s == ','  = (reverse name,ss)
    | s == '"'  = readName (name,ss)
    | otherwise = readName (s:name,ss)

splitInput :: [String] -> String -> [String]
splitInput names "" = L.sort names
splitInput names ss =
    let (nm,ss') = readName ("", ss)
    in splitInput (nm:names) ss'

splitInput' :: String -> [String]
splitInput' input = splitInput [] input


bigAOffset = (ord 'A') - 1
letterVal c = (ord c) - bigAOffset
wordVal w = foldr (\c z -> letterVal c + z) 0 w
