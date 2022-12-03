toPriority :: Char -> Int
toPriority c
  | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a' + 1
  | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A' + 27

splitAtMiddle :: [a] -> ([a], [a])
splitAtMiddle xs = splitAt (length xs `div` 2) xs

findError :: String -> String -> String
findError l r = filter (`elem` l) r

main :: IO ()
main = do
  let fileName = "input"

  sum . fmap (toPriority . head . uncurry findError . splitAtMiddle) . lines
    <$> readFile fileName
    >>= putStrLn . show
