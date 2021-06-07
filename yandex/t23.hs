main = do
  size <- read <$> getLine :: IO [Int]
  (x:xs) <- (map read . words) <$> getLine
  num <- read <$> getLine
  print $
    foldr
      (\x acc ->
         if abs (num - acc) > abs (x - num)
           then x
           else acc)
      x
      (x : xs)
