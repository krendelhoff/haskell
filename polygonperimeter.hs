import           Text.Printf (printf)

type Point a = (a, a)

dist :: Point Int -> Point Int -> Double
dist (x1, y1) (x2, y2) =
  sqrt $ (fromIntegral (x1 - x2) ^ 2) + (fromIntegral (y1 - y2) ^ 2)

calcPerimeter :: [Point Int] -> Point Int -> Double -> Double
calcPerimeter [x] h acc      = dist x h + acc
calcPerimeter (x:y:xs) h acc = calcPerimeter (y : xs) h (acc + dist x y)

calcArea :: [Point Int] -> Double
calcArea lst = (abs $ traverse lst (head lst) 0) / 2
  where
    traverse :: [Point Int] -> Point Int -> Double -> Double
    traverse [x] h acc      = det x h + acc
    traverse (x:y:xs) h acc = traverse (y : xs) h (acc + det x y)
    det (x1, y1) (x2, y2) = fromIntegral $ x1 * y2 - x2 * y1

main = do
  n <- readLn
  lst <- sequence $ replicate n getLine
  let points :: [Point Int]
      points =
        map (\[x, y] -> (x, y)) $
        map (map (read :: String -> Int)) $ map words lst
  printf "%.1f\n" $ calcArea points
