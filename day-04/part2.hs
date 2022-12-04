import Data.Functor ((<&>))

data Range = Range Int Int deriving (Show)

overlaps :: Range -> Range -> Bool
(Range a b) `overlaps` (Range c d) = a <= d && b >= c

splitOnDelimeters :: String -> [String]
splitOnDelimeters = f [] ""
  where
    f splits cur "" = reverse (map reverse (cur : splits))
    f splits cur (x : xs) | x == ',' || x == '-' = f (cur : splits) "" xs
    f splits cur (x : xs) = f splits (x : cur) xs

asRanges :: [Int] -> (Range, Range)
asRanges [a, b, c, d] = (Range a b, Range c d)

main :: IO ()
main = do
  let fileName = "input"

  readFile fileName
    <&> length
      . filter (uncurry overlaps)
      . fmap (asRanges . map (read :: String -> Int) . splitOnDelimeters)
      . lines
    >>= putStrLn . show
