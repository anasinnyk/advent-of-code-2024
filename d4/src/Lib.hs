module Lib (main, solve, solve') where

import System.IO.Unsafe (unsafePerformIO)
import Data.List (transpose)
import Data.Text (breakOnAll, pack, unpack, Text)
import Data.Tuple.Extra (fst3)

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

inputData :: String
inputData = unsafePerformIO $ readFile inputFile

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
    sum [
      findXmas ss 
    , findXmas (transpose ss) 
    , findXmas (map reverse ss) 
    , findXmas (map reverse $ transpose ss) 
    , findXmas (diagonals ss)
    , findXmas (diagonals $ transpose ss) 
    , findXmas (diagonals $ map reverse ss) 
    , findXmas (diagonals $ transpose $ map reverse ss) 
    ]


correct :: [String] -> Bool
correct ss = ss !! 1 !! 1 == 'A'
        && (
             (ss !! 0 !! 0 == 'M' && ss !! 0 !! 2 == 'M' && ss !! 2 !! 2 == 'S' && ss !! 2 !! 0 == 'S')
          || (ss !! 0 !! 0 == 'S' && ss !! 0 !! 2 == 'S' && ss !! 2 !! 2 == 'M' && ss !! 2 !! 0 == 'M')
          || (ss !! 0 !! 0 == 'S' && ss !! 0 !! 2 == 'M' && ss !! 2 !! 2 == 'M' && ss !! 2 !! 0 == 'S')
          || (ss !! 0 !! 0 == 'M' && ss !! 0 !! 2 == 'S' && ss !! 2 !! 2 == 'S' && ss !! 2 !! 0 == 'M')
        )

fstCorrect ss = fst3 (ss !! 1 !! 1) == 'A'
        && (
             (fst3 (ss !! 0 !! 0) == 'M' && fst3 (ss !! 0 !! 2) == 'M' && fst3 (ss !! 2 !! 2) == 'S' && fst3 (ss !! 2 !! 0) == 'S')
          || (fst3 (ss !! 0 !! 0) == 'S' && fst3 (ss !! 0 !! 2) == 'S' && fst3 (ss !! 2 !! 2) == 'M' && fst3 (ss !! 2 !! 0) == 'M')
          || (fst3 (ss !! 0 !! 0) == 'S' && fst3 (ss !! 0 !! 2) == 'M' && fst3 (ss !! 2 !! 2) == 'M' && fst3 (ss !! 2 !! 0) == 'S')
          || (fst3 (ss !! 0 !! 0) == 'M' && fst3 (ss !! 0 !! 2) == 'S' && fst3 (ss !! 2 !! 2) == 'S' && fst3 (ss !! 2 !! 0) == 'M')
        )

submatrics3x3 :: [[a]] -> [[[a]]]
submatrics3x3 mat = [map (take 3) (take 3 (drop r (map (drop c) mat))) | r <- [0..length mat - 3], c <- [0..length (head mat) - 3]]

imat mat = map (\(i, xs) -> map (\(j,x) -> (x,i,j)) xs) $ zip [0..] $ map (zip [0..]) mat

-- | solve'
-- >>> solve' testData
-- 9
solve' :: String -> Int
solve' s = length $ filter correct $ submatrics3x3 $ lines s
