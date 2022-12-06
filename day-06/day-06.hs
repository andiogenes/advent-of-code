import Data.List (find, nub)

get (Just x) = x

findMarker offset buf = (+ offset) . fst . get $ find (uniqueWindow . snd) windows
  where
    uniqueWindow = (== offset) . length . nub
    windows = [(i, take offset $ drop i buf) | i <- enumFromTo 0 $ len - offset]
    len = length buf

main :: IO ()
main = do
  let fileName = "input"
  let content = readFile fileName

  -- Part 1
  content >>= return . findMarker 4 >>= print

  -- Part 2
  content >>= return . findMarker 14 >>= print
