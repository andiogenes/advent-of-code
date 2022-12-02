data Result = Won Int | Draft Int | Lose Int

data Hand = R | P | S deriving Eq

parseHand :: Char -> Hand
parseHand c
  | c == 'A' || c == 'X' = R
  | c == 'B' || c == 'Y' = P
  | c == 'C' || c == 'Z' = S

doRound :: String -> Int
doRound (x : ' ' : y : []) = estimate $ resolve (parseHand x) (parseHand y)
  where
    resolve S R = Won 1
    resolve R P = Won 2
    resolve P S = Won 3
    resolve their own | their == own = Draft $ decode own
    resolve _ own = Lose $ decode own

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
