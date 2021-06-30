nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN _ n _
  | n < 0 = []
nextPositionsN b 0 pred
  | pred b = return b
  | otherwise = []
nextPositionsN b n pred = do
  p <- nextPositions b
  nextPositionsN p (n - 1) pred
