import           Data.List

highestRank :: (Ord c) => [c] -> c
highestRank =
  fst .
  head .
  sortBy
    (\(x1, y1) (x2, y2) ->
       if (y1, x1) > (y2, x2)
         then LT
         else GT) .
  map (\x -> (head x, length x)) .
  group .
  sortBy
    (\x y ->
       if x > y
         then LT
         else GT)

main = print $ highestRank [12, 10, 8, 12, 7, 6, 4, 10, 12]
