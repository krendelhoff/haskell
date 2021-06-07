import           Data.List

coins =
  [2, 3, 7] :: (Ord a, Num a) =>
                 [a]

change :: (Ord a, Num a) => a -> [[a]]
change money = sort . nub . filter (\x -> sum x == money) . helper [[]] $ money
  where
    helper :: (Ord a, Num a) => [[a]] -> a -> [[a]]
    helper acc money
      | all (>= money) $ map sum acc = acc
      | otherwise =
        helper
          [ if sum y < money
            then (fromIntegral x) : y
            else y
          | y <- acc
          , x <- coins
          ]
          money

main = print $ change 7
