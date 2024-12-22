import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)

inputFile = "d1/input.txt"
testFile = "d1/test.txt"

main :: IO ()
main = do
  process inputFile solve
  process inputFile solve'

process :: String -> (String -> Int) -> IO ()
process file f = do
  input <- readFile file
  putStrLn $ show $ f input

-- | solve
-- >>> solve (unsafePerformIO $ readFile testFile)
-- 11
--
solve :: String -> Int
solve s = sum $ distances $ joinLists $ sortLists $ buildInput s

-- | solve'
-- >>> solve' (unsafePerformIO $ readFile testFile)
-- 31
solve' :: String -> Int
solve' s = sum $ res $ buildInput s
  where res :: ([Int], [Int]) -> [Int]
        res ([], _) = []
        res ((x:xs), ys) = (x * findEntry x ys):res (xs, ys)

findEntry :: Int -> [Int] -> Int
findEntry n xs = foldl (\acc x -> acc + (if x == n then 1 else 0)) 0 xs

buildInput :: String -> ([Int], [Int])
buildInput s = unzip $ map (\(x:y:_) -> (x, y)) $ map parse $ map words $ lines s

parse :: [String] -> [Int]
parse = map read

sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (xs,ys) = (sort xs, sort ys)

joinLists :: ([Int], [Int]) -> [(Int, Int)]
joinLists (xs, ys) = zip xs ys

distances :: [(Int, Int)] -> [Int]
distances = map (\(x, y) -> abs (x - y))


