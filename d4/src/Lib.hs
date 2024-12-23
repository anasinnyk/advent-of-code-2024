module Lib (main, solve, solve') where

import System.IO.Unsafe (unsafePerformIO)
import Data.List (transpose)
import Data.Text (breakOnAll, pack, unpack, Text)

inputFile = "data/input.txt"
testFile = "data/test.txt"

main :: IO ()
main = do
  process inputFile solve
  putStrLn "==="
  process inputFile solve'

process :: String -> (String -> Int) -> IO ()
process file f = do
  input <- readFile file
  putStrLn $ show $ f input

testData :: String
testData = unsafePerformIO $ readFile testFile

neadle :: String
neadle = "XMAS"

diagonals :: [[a]] -> [[a]]
diagonals m = top ++ bottom
  where
    rows = length m
    cols = length (head m)
    top = [[m !! r !! c | (r,c) <- zip [0..] [i, i-1..0], r < rows, c < cols] | i <- [0..cols-1]]
    bottom = [[m !! r !! c | (r,c) <- zip [i..] [cols-1, cols-2..0],r < rows, c >= 0] | i <- [1..rows-1]]

findXmas :: [String] -> Int
findXmas ss = sum $ map length $ map (\x -> breakOnAll (pack neadle) (pack x)) ss

-- | solve
-- >>> solve testData
-- 18
solve :: String -> Int
solve s = let
  ss = lines s
  in 
      findXmas ss 
    + findXmas (transpose ss) 
    + findXmas (map reverse ss) 
    + findXmas (map reverse $ transpose ss) 
    + findXmas (diagonals ss)
    + findXmas (diagonals $ transpose ss) 
    + findXmas (diagonals $ map reverse ss) 
    + findXmas (diagonals $ transpose $ map reverse ss) 

solve' :: String -> Int
solve' = undefined
