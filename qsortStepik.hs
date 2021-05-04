qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let leftSide = [y | y <- xs, y <= x]
      rightSide = [y | y <- xs, y > x]
   in qsort leftSide ++ [x] ++ rightSide

main = print $ [9,8 .. 0]
