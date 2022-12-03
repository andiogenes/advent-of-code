-- https://stackoverflow.com/a/12882583
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

toPriority :: Char -> Int
toPriority c
  | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a' + 1
  | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A' + 27

findError :: String -> String -> String
findError l r = filter (`elem` l) r

findBadge :: [String] -> String
findBadge (x : xs) = foldr findError x xs

main :: IO ()
main = do
  let fileName = "input"

  sum . fmap (toPriority . head . findBadge) . chunks 3 . lines
    <$> readFile fileName
    >>= putStrLn . show
