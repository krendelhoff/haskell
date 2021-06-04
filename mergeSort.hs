mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort lst = merge (mSort p1) (mSort p2)
  where
    len = length lst
    (p1, p2) = splitAt (len `div` 2) lst
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) =
      if x > y
        then y : merge (x : xs) ys
        else x : merge xs (y : ys)
