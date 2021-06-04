calc :: Int -> Int -> Int -> Int -> Int
calc n k m acc
  | n < k || m > k = acc
calc n k m acc = calc remainder k m ((+) details $! acc)
  where
    rest = n `mod` k
    blankQuan = n `div` k
    detailsPerBlank = k `div` m
    details = detailsPerBlank * blankQuan
    remPerBlank = k `mod` m
    remainder = rest + remPerBlank * blankQuan

main = do
  [n, k, m] <- (map read . words) <$> getLine
  print $ calc n k m 0
