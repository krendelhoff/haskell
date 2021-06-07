xbonacci :: Num a => [a] -> Int -> [a]
xbonacci [] _ = []
xbonacci as n
  | n <= len = take n as
  | otherwise = (take (len - 1) as ++ xbonacciHelper as (n - len + 1) 0 [])
  where
    len = length as
    xbonacciHelper as end n acc
      | n == end = acc
      | otherwise =
        xbonacciHelper ((drop 1 as) ++ [sum as]) end (n + 1) (acc ++ [last as])

main :: IO ()
main = print $ xbonacci [1, 1, 1, 1] 10
