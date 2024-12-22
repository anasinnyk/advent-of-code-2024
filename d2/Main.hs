import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM2)

inputFile = "d2/input.txt"
testFile = "d2/test.txt"

main :: IO ()
main = do
  process inputFile solve
  putStrLn "==="
  process inputFile solve'

process :: (Show a) => String -> (String -> a) -> IO ()
process file f = do
  input <- readFile file
  putStrLn $ show $ f input

-- | solve
-- >>> solve (unsafePerformIO $ readFile testFile)
-- 2
--
solve :: String -> Int
solve s = length $ filter (liftM2 (||) isReportCorrect (isReportCorrect . reverse)) $ parseInput s

-- | solve'
-- >>> solve' (unsafePerformIO $ readFile testFile)
-- 4
--
solve' :: String -> Int
solve' s = length $ filter (liftM2 (||) isReportCorrect' (isReportCorrect' . reverse)) $ parseInput s

isReportCorrect' :: [Int] -> Bool
isReportCorrect' l = isReportCorrect left || isReportCorrect right
    where res xs = map (liftM2 (&&) (<4) (>0)) $ map (uncurry (-)) $ zip xs (tail xs)
          left = head l : (correction $ zip (tail l) $ res l)
          right = (correction $ zip l $ res l) ++ [last l]

correction :: [(Int, Bool)] -> [Int]
correction [] = []
correction ((x, b):xs) = if b then x:correction xs else (map fst xs)

isReportCorrect :: [Int] -> Bool
isReportCorrect xs = all (liftM2 (&&) (<4) (>0)) $ map (uncurry (-)) $ zip xs (tail xs)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines
