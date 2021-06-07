count :: [Int] -> Int
count = (flip countH) 0
  where
    countH [] count = count
    countH [x] count = count
    countH [x, y] count = count
    countH (x:y:z:xs) count =
      if y > x && y > z
        then countH (y : z : xs) ((+) 1 $! count)
        else countH (y : z : xs) count

main = do
  (count . map read . words) <$> getLine >>= print
