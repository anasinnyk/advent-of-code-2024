module Lib (solve) where

import System.IO.Unsafe (unsafePerformIO)
import GHC.Utils.Misc (countWhile, split)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative ((<|>))

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

-- | solve
-- >>> solve testData
-- 143
solve :: String -> Int
solve s = sum $ map (\u -> u !! (length u `div` 2)) correctUpdates 
  where
    (rs, updates) = parse s
    rules = rulesMap rs
    correctUpdates = filter (and . (checkKeys rules)) $ updates

solve' :: String -> Int
solve' = undefined

rulesMap :: [(Int, Int)] -> Map Int [Int]
rulesMap = foldl (\m (k,v) -> M.alter (flip (<|>) (Just [v]) . fmap (v :)) k m) M.empty

checkKeys :: M.Map Int [Int] -> [Int] -> [Bool]
checkKeys _ [] = []
checkKeys m (x:xs) = maybe (length xs == 0) (\ys -> and (map (\x' -> x' `elem` ys) xs)) (M.lookup x m) : checkKeys m xs

parse :: String -> ([(Int, Int)], [[Int]])
parse s = (rules, updates)
  where
    ls = lines s
    rulesCount = countWhile (/= "") ls
    rulesSrts = take rulesCount ls
    updatesStrs = drop (rulesCount+1) ls
    rules = map (\(x:y:_) -> (x, y)) $ map (map read . split '|') rulesSrts
    updates = map (map read . split ',') updatesStrs


