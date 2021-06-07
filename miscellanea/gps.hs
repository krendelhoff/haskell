gps :: Int -> [Double] -> Int
gps _ [] = 0
gps _ [_] = 0
gps s x = floor $ maximum (createList s x [])
  where
    createList s (x:xs) acc
      | null xs = acc
      | otherwise =
        createList s xs ((3600 * abs (x - head xs) / (fromIntegral s)) : acc)

main :: IO ()
main = print $ gps 15 [0.0, 0.19, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25]
