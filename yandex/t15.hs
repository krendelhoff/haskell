import           Data.List
import           Text.Printf

printError :: IO ()
printError = printf "-1 -1\n"

calcn _ 0 _ 0    = 0
calcn k2 p2 m n2 = k2 `div` (p2 * m + n2)

calcFloorEntrance k1 m n =
  let p1 = k1 `div` (m * n)
      relk = k1 - p1 * m * n
   in (p1, relk `div` n)

possibleN k2 n2 k1 p2 m = possibleForN
  where
    possibleForN = filter checkValid [1 .. (max k1 k2)]
    checkValid n =
      let diff = abs (p2 * m * n + n2 * n - k2)
       in diff >= 0 &&
          diff < n &&
          (case n2 == 0 of
             True  -> n >= k2 - p2 * m * n
             False -> k2 - p2 * m * n - (n2 - 1) * n >= n)

result :: [(Int, Int)] -> (Int, Int)
result lst = (res l1, res l2)
  where
    (l1, l2) = unzip lst
    res l =
      if (length . nub $ l) == 1
        then head l
        else -1

main = do
  input <- (map read . words) <$> getLine
  let lst@[k1, m, k2, p2, n2] =
        zipWith
          ($)
          [(\x -> x - 1), id, (\x -> x - 1), (\x -> x - 1), (\x -> x - 1)]
          input
  let lst = map (calcFloorEntrance k1 m) $ possibleN k2 n2 k1 p2 m
  if n2 >= m
    then printError
    else if (p2 == 0 && n2 == 0 && k1 < k2)
           then printf "1 1\n"
           else if null lst
                  then printError
                  else let (p1, n1) = result lst
                        in printf "%d %d\n" (p1 + 1) (n1 + 1)
