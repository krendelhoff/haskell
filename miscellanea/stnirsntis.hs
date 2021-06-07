avg :: Int -> Int -> Int -> Double
avg a b c =
  (((fromIntegral a) :: Double) + ((fromIntegral b) :: Double) +
   ((fromIntegral c) :: Double)) /
  3
