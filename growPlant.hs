growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed = helper 0 0 upSpeed
  where
    helper h n u d dH
      | dH <= h = n
      | dH <= (h + u) = n + 1
      | otherwise = helper (h + u - d) (n + 1) u d dH

main :: IO ()
main = print $ growingPlant 5 2 5
