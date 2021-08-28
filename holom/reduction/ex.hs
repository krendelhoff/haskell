{-# LANGUAGE BangPatterns #-}

sum2 :: [Int] -> (Int, Int)
sum2 = iter (0, 0)
  where
    iter c []     = c
    iter c (x:xs) = iter (tick x c) xs

tick :: Int -> (Int, Int) -> (Int, Int)
tick x (!c0, !c1)
  | even x = (c0, c1 + 1)
  | otherwise = (c0 + 1, c1)
