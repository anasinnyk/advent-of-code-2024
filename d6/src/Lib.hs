module Lib (solve) where

import Data.Map (Map)
import Data.Map qualified as M
import System.IO.Unsafe (unsafePerformIO)

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

data Direction = N | E | S | W deriving (Show, Eq)

turn :: Direction -> Direction
turn N = E
turn E = S
turn S = W
turn W = N

next :: (Int, Int) -> Direction -> (Int, Int)
next (x, y) N = (x - 1, y)
next (x, y) E = (x, y + 1)
next (x, y) S = (x + 1, y)
next (x, y) W = (x, y - 1)

{- | solve
>>> solve testData
-}
solve :: String -> Int
solve s = length $ M.filter (== 'X') $ move (field s) N $ firstPosition $ field s

{- | solve'
>>> solve testData
-}
solve' :: String -> Int
solve' _ = undefined

field :: String -> Map (Int, Int) Char
field s = M.fromList [((x, y), ls !! x !! y) | x <- [0 .. cs], y <- [0 .. lls]]
 where
  ls = lines s
  cs = length ls - 1
  lls = (length $ head ls) - 1

firstPosition :: Map (Int, Int) Char -> (Int, Int)
firstPosition m = fst $ head $ M.toList $ M.filter (== '^') m

move :: Map (Int, Int) Char -> Direction -> (Int, Int) -> Map (Int, Int) Char
move m d p =
  let
    updated = M.insert p 'X' m
    nextp = next p d
   in
    case M.lookup nextp m of
      Just '.' -> move updated d nextp
      Just 'X' -> move updated d nextp
      Just '#' -> move updated (turn d) (next p (turn d))
      Nothing -> updated
