sumEven :: (Integral a) => [a] -> a
sumEven =
  foldr
    (\x acc ->
       if even x
         then acc + 1
         else acc)
    0

lstOdd :: (Integral a) => [a] -> [a]
lstOdd =
  foldr
    (\x acc ->
       if odd x
         then x : acc
         else acc)
    []

revAdj =
  concat .
  map
    (\p ->
       case p of
         [x, y] -> [y, x]
         [x]    -> [x]) .
  groupBy 2
  where
    groupBy n []  = []
    groupBy n lst = take n lst : groupBy n (drop n lst)

sumTwo = zipWith (+)

takeNFromK n k = take n . drop k

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
