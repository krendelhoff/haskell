sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count n = helper (0, 0) n
  where
    helper p 0 = p
    helper (a, b) n = helper (a + n `mod` 10, b + 1) (n `div` 10)
