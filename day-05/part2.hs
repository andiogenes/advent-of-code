import Control.Monad (forM_)
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.List (elemIndex, transpose)

justElemIndex x xs = f $ elemIndex x xs
  where
    f (Just x) = x

splitWhile p xs = f ([], xs)
  where
    f (xs, []) = (reverse xs, [])
    f (xs, xss) | not $ p (head xss) = (reverse xs, xss)
    f (xs, (y : ys)) = f (y : xs, ys)

isDigit x = x >= '0' && x <= '9'

splitInput :: [String] -> ([String], [String])
splitInput xs = (ys, tail zs)
  where
    (ys, zs) = splitAt idx xs
    idx = justElemIndex "" xs

parseStacks = fmap (reverse . snd . splitWhile isDigit . filter (/= ' ')) . filter (not . delimiter) . transpose . reverse
  where
    delimiter x = head x == ' '

parseComponent :: String -> Writer [Int] String
parseComponent s = do
  let (n, s') = splitWhile isDigit . snd . splitWhile (not . isDigit) $ s
  writer (s', [read n])

parseCommands :: [String] -> [(Int, Int, Int)]
parseCommands cs = fmap parseString cs
  where
    parseString = asTuple . execWriter . (parseComponent >=> parseComponent >=> parseComponent)
    asTuple [x, y, z] = (x, y, z)

update :: Int -> a -> [a] -> [a]
update n ne xs = take n xs ++ [ne] ++ drop (n + 1) xs

execute :: [String] -> [(Int, Int, Int)] -> [String]
execute stacks commands = (`execState` stacks) $ do
  forM commands $ \(move, from, to) -> do
    s' <- get

    let (from', to') = (pred from, pred to)
    let (top, fromAfter) = splitAt move $ s' !! from'
    let toAfter = top ++ s' !! to'

    modify $ update to' toAfter . update from' fromAfter

main :: IO ()
main = do
  let fileName = "input"

  (ss, cs) <- readFile fileName <&> splitInput . lines

  let stacks = parseStacks ss
  let commands = parseCommands cs

  putStrLn $ execute stacks commands >>= return . head
