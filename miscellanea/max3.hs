max3 :: (Ord a) => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max z (max x y))

main = print $ max3 "AXZ" "YDW" "MLK"
