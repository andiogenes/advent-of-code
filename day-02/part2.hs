data Result = Won Int | Draft Int | Lose Int

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
doRound (x : ' ' : y : []) = estimate $ resolve (parseHand x) (parseHint y)
  where
    resolve h L = Lose . decode . predHand $ h
    resolve h D = Draft . decode $ h
    resolve h W = Won . decode . succHand $ h

    decode R = 1
    decode P = 2
    decode S = 3

    estimate (Won x) = x + 6
    estimate (Draft x) = x + 3
    estimate (Lose x) = x

main :: IO ()
main = do
  let fileName = "input"

  content <- readFile fileName

  (putStrLn . show . sum) $ doRound <$> lines content
