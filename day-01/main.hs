import Data.Ord (Down (..))
import Data.List (sortOn)
import Text.Read (readMaybe)

calculateCalories :: [Maybe Int] -> [Int]
calculateCalories = f [] 0
  where f c s [] = c
        f c s (Nothing:xs) = f (s:c) 0 xs
        f c s ((Just x):xs) = f c (s + x) xs

main :: IO ()
main = do
  let fileName = "input"

  content <- readFile fileName
  let ls = lines content

  let caloriesOpt = (readMaybe <$> ls) :: [Maybe Int]
  let calories = calculateCalories caloriesOpt

  -- I know it could be done better
  let top3 = (sum . take 3 . sortOn Down) calories

  putStrLn $ show top3
