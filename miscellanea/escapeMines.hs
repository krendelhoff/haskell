type XY = (Int, Int)

type Grid = [[Bool]]

type Visited = [XY]

data Move
  = U
  | D
  | R
  | L
  deriving (Eq, Show)

solve :: Grid -> XY -> XY -> [Move]
solve grid miner exit =
  case dfs [(miner, [])] [] of
    (Just path) -> reverse path
  where
    lenCol = length $ head grid
    lenRow = length grid
    move :: XY -> Move -> XY
    move (x, y) U = (x, y - 1)
    move (x, y) D = (x, y + 1)
    move (x, y) R = (x + 1, y)
    move (x, y) L = (x - 1, y)
    at :: Grid -> XY -> Bool
    grid `at` (x, y) = grid !! x !! y
    checkBound :: XY -> Bool
    checkBound (x, y) = x >= 0 && x < lenRow && y >= 0 && y < lenCol
    allDirections = [U, D, R, L]
    dfs :: [(XY, [Move])] -> Visited -> Maybe [Move]
    dfs [] _ = Nothing
    dfs ((pos, path):xs) visited
      | pos == exit = Just path
      | otherwise =
        let neighbors = [(move pos dir, dir : path) | dir <- allDirections]
            legalOnes =
              [ (pos, path)
              | (pos, path) <- neighbors
              , checkBound pos
              , grid `at` pos
              , not (pos `elem` visited)
              ]
         in dfs (legalOnes ++ xs) (pos : visited)
