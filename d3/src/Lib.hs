module Lib (main, solve) where

import System.IO.Unsafe (unsafePerformIO)
import Text.Regex (mkRegex, matchRegex, matchRegexAll, Regex)

inputFile = "data/input.txt"
testFile = "data/test.txt"

main :: IO ()
main = do
  process inputFile solve

process :: String -> (String -> Int) -> IO ()
process file f = do
  input <- readFile file
  putStrLn $ show $ f input

regex :: Regex
regex = mkRegex "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

-- | solve
-- >>> solve (unsafePerformIO $ readFile testFile)
-- 161
--
solve :: String -> Int
solve s = sum $ map (uncurry (*)) $ buildPairs s

buildPairs :: String -> [(Int, Int)]
buildPairs = res []
  where 
    res acc s = case matchRegexAll regex s of
      Just (_, _, str, (x:y:_)) -> res ((read x, read y):acc) str
      Nothing -> acc

