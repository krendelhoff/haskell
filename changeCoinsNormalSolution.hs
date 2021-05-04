coins =
  [2, 3, 7] :: (Ord a, Num a) =>
                 [a]

change :: (Ord a, Num a) => a -> [[a]]
change n
  | n < 0 = []
  | n == 0 = [[]]
  | otherwise =
    [(fromIntegral x) : xs | x <- coins, xs <- change (n - fromIntegral x)]

-- это очень очевидно, в след. раз подумаем лучше
--
main = print $ change 7
