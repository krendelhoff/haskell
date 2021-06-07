integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b
  | a == b = 0
  | otherwise =
    sum $
    map
      (\section -> trapezoidArea f section)
      (makeFragmentation (fragmentate a b) [])
  where
    fragmentate a b = [a,(a + (b - a) / 1000) .. b]
    makeFragmentation list acc
      | null list || null (tail list) = acc
      | otherwise = makeFragmentation (drop 1 list) ((take 2 list) : acc)
    trapezoidArea f (x:y:[]) = ((f y) + (f x)) * (y - x) / 2

main :: IO ()
main = do
  print (integration sin pi 0)
