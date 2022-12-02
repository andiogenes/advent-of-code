data Hand = R | P | S deriving (Enum, Eq)

data Hint = L | W | D

succHand :: Hand -> Hand
succHand S = R
succHand x = succ x

predHand :: Hand -> Hand
predHand R = S
predHand x = pred x

parseHand :: Char -> Hand
parseHand 'A' = R
parseHand 'B' = P
parseHand 'C' = S

parseHint :: Char -> Hint
parseHint 'X' = L
parseHint 'Y' = D
parseHint 'Z' = W

doRound :: String -> Int
doRound (x : ' ' : y : []) = score (parseHint y) (parseHand x)
  where
    score L = decode . predHand
    score D = (+ 3) . decode
    score W = (+ 6) . decode . succHand

    decode R = 1
    decode P = 2
    decode S = 3

main :: IO ()
main = do
  let fileName = "input"

  content <- readFile fileName

  (putStrLn . show . sum) $ doRound <$> lines content
