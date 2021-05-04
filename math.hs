main =
  print
    [x | x <- [-200 .. 350], (foldr (-) x [2, 1, 5]) == (foldl (-) x [2, 1, 5])]
