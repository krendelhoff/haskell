import           Data.List

nextBigger :: Int -> Int
nextBigger n = undefined
  where
    result str _ | ((read str) :: Int) < 10 = -1
    result 

-- идею понял - идем с конца, как только нашли что можно свапнуть - свапаем и возвращаем. Никакой explicit recursion, подумай потом как реализовать через правую свертку

main = print $ nextBigger (-521)
