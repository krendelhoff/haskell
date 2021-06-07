comb :: Int -> Int -> [[Int]]
comb 0 _ = []
comb _ 0 = [[]]
comb n k = [y ++ [x] | y <- comb n $ k - 1, x <- [1 .. n], null y || last y < x]

cmbs :: Int -> Int -> Int
cmbs n k = product [1 .. n] `div` (product [1 .. k] * product [1 .. (n - k)])

main = do
  k <- readLn
  print $ comb 10 k
