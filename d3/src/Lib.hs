module Lib (main, solve) where

import System.IO.Unsafe (unsafePerformIO)
import Text.Regex (mkRegex, matchRegex, matchRegexAll, Regex)

inputFile = "data/input.txt"
testFile = "data/test.txt"
test2File = "data/test2.txt"

main :: IO ()
main = do
  process inputFile solve
  putStrLn "==="
  process inputFile solve2

process :: String -> (String -> Int) -> IO ()
process file f = do
  input <- readFile file
  putStrLn $ show $ f input

regex :: Regex
regex = mkRegex "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

regex2 :: Regex
regex2 = mkRegex "(mul|do|don't)\\((([0-9]{1,3}),([0-9]{1,3}))?\\)"

-- | solve
-- >>> solve (unsafePerformIO $ readFile testFile)
-- 161
--
solve :: String -> Int
solve s = sum $ map (uncurry (*)) $ buildPairs s

-- | solve2
-- >>> solve2 (unsafePerformIO $ readFile test2File)
-- 48
solve2 :: String -> Int
solve2 s = sum $ map (uncurry (*)) $ buildPairs' s

buildPairs' :: String -> [(Int, Int)]
buildPairs' = res [] True
  where 
    res acc t s = case matchRegexAll regex2 s of
      Just (_, _, str, (x:_:i:j:_)) -> case x of
        "do" -> res acc True str
        "don't" -> res acc False str
        "mul" -> if t 
          then res ((read i, read j):acc) t str 
          else res acc t str
      Nothing -> acc

buildPairs :: String -> [(Int, Int)]
buildPairs = res []
  where 
    res acc s = case matchRegexAll regex s of
      Just (_, _, str, (x:y:_)) -> res ((read x, read y):acc) str
      Nothing -> acc

