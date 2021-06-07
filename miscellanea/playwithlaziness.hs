type Matrix = [[Bool]]

generate :: Int -> Int -> Matrix
generate n m =
  let falses = repeat False
      oneTrue = replicate m False ++ [True] ++ falses
   in replicate n falses ++ [oneTrue] ++ repeat falses

findTrue :: Matrix -> (Int, Int)
findTrue m = findPoint 0
  where
    generatePoints 0 = [(0, 0)]
    generatePoints n =
      [(x, n) | x <- [0 .. (n - 1)]] ++ [(n, x) | x <- [0 .. n]]
    checkIn =
      foldr
        (\(x, y) acc ->
           case acc of
             Nothing ->
               if m !! x !! y
                 then Just (x, y)
                 else Nothing
             Just (x, y) -> Just (x, y))
        Nothing
    findPoint n =
      case checkIn $ generatePoints n of
        Nothing      -> findPoint (n + 1)
        (Just point) -> point
