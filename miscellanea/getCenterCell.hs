data Coord a =
  Coord a a
  deriving (Eq, Show)

getCenter :: Double -> Coord Int -> Coord Double
getCenter cellSize (Coord a b) =
  Coord (cellSize * (newa + 1 / 2)) (cellSize * (newb + 1 / 2))
  where
    newa = fromIntegral a
    newb = fromIntegral b

getCell :: Double -> Coord Double -> Coord Int
getCell cellSize (Coord a b) =
  Coord (floor (cellSize ** (-1) * a)) (floor (cellSize ** (-1) * b))

main = print $ getCell 2.2 (Coord 3.2 1.6)
