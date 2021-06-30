import           Data.List
import           Text.Printf

findTwoMax :: [Int] -> (Int, Int)
findTwoMax (x:y:xs) = fTM (max x y) (min x y) xs
  where
    fTM x y [] = (x, y)
    fTM x y (z:zs)
      | y >= z = fTM x y zs
      | z > y && z < x = fTM x z zs
      | z >= x = fTM z x zs

findTwoMin :: [Int] -> (Int, Int)
findTwoMin (x:y:xs) = fTM (min x y) (max x y) xs
  where
    fTM x y [] = (x, y)
    fTM x y (z:zs)
      | y <= z = fTM x y zs
      | z > x && z < y = fTM x z zs
      | z <= x = fTM z x zs

main = do
  lst <- (map read . words) <$> getLine
  let (a, b) = findTwoMax lst
      (c, d) = findTwoMin lst
  if a * b > c * d
    then printf "%d %d\n" b a
    else printf "%d %d\n" c d
