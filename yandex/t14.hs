import           Control.Monad

solve :: Int -> Int -> Int -> Either String Int
solve a b c
  | c < 0 = Left "NO SOLUTION"
  | a == 0 =
    if c * c == b
      then Left "MANY SOLUTIONS"
      else Left "NO SOLUTION"
  | otherwise =
    if (c * c - b) `mod` a == 0
      then Right $ (c * c - b) `div` a
      else Left "NO SOLUTION"

main = do
  [a, b, c] <- map read <$> replicateM 3 getLine
  case solve a b c of
    Left s  -> putStrLn s
    Right x -> print x
