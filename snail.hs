import           Data.List

snail :: [[Int]] -> [Int]
snail [[]] = []
snail array = snailHelper 0 array
  where
    getRightCorner array@(x:xs) = (init $ x) ++ (last $ transpose array)
    getLeftCorner array =
      (reverse $ tail $ last array) ++ (reverse $ head $ transpose array)
    cutRight array = tail $ map (\x -> init x) array
    cutLeft array = init (map (\x -> tail x) array)
    snailHelper _ [[x]] = [x]
    snailHelper n array
      | n `mod` 2 == 0 =
        (getRightCorner array) ++ (snailHelper (n + 1) (cutRight array))
      | otherwise =
        (getLeftCorner array) ++ (snailHelper (n + 1) (cutLeft array))

-- суть была в том чтобы увидеть здесь рекурсию и декларативность, что ты и сделал
array = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

main = print $ snail array
  {-
    123
    456
    789 -}
