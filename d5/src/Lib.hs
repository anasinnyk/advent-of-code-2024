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

-- | solve'
-- >>> solve' testData
-- 123
solve' :: String -> Int
solve' s = sum $ map (\u -> u !! (length u `div` 2)) onlyIncorrect
  where
    (rs, updates) = parse s
    rules = rulesMap rs
    updatesCorection = map (correctUpdates rules) updates
    onlyIncorrect = map fst $ filter (uncurry (/=)) $ zip updatesCorection updates

rulesMap :: [(Int, Int)] -> Map Int [Int]
rulesMap = foldl (\m (k,v) -> M.alter (flip (<|>) (Just [v]) . fmap (v :)) k m) M.empty

checkKeys :: M.Map Int [Int] -> [Int] -> [Bool]
checkKeys _ [] = []
checkKeys m [x] = [True]
checkKeys m (x:xs) = maybe False (\ys -> and (map (\x' -> x' `elem` ys) xs)) (M.lookup x m) : checkKeys m xs

correctUpdates :: M.Map Int [Int] -> [Int] -> [Int]
correctUpdates _ [] = []
correctUpdates m [x] = [x]
correctUpdates m (x:xs) = case M.lookup x m of
  Just ys -> if and (map (`elem` ys) xs) then x: correctUpdates m xs else correctUpdates m (xs ++ [x])
  Nothing -> correctUpdates m (xs ++ [x])

parse :: String -> ([(Int, Int)], [[Int]])
parse s = (rules, updates)
  where
    ls = lines s
    rulesCount = countWhile (/= "") ls
    rulesSrts = take rulesCount ls
    updatesStrs = drop (rulesCount+1) ls
    rules = map (\(x:y:_) -> (x, y)) $ map (map read . split '|') rulesSrts
    updates = map (map read . split ',') updatesStrs


