import           Data.List
import           Text.Printf

findTwoMax :: [Int] -> (Int, Int)
findTwoMax [] = (undefined, undefined)
findTwoMax [x] = (undefined, undefined)
findTwoMax (x:y:xs) = properTuple $ findTwoMaxH (x, y) xs
  where
    findTwoMaxH (x, y) [] = (x, y)
    findTwoMaxH (x, y) (z:zs) =
      findTwoMaxH
        (snd .
         maximumBy
           (\t1@(p1, _) t2@(p2, _) ->
              if p1 > p2
                then GT
                else LT) $
         [(x * y, (x, y)), (y * z, (y, z)), (x * z, (x, z))])
        zs
    properTuple (a, b) =
      if a > b
        then (b, a)
        else (a, b)

main = do
  lst <- (map read . words) <$> getLine
  let (a, b) = findTwoMax lst
  printf "%d %d\n" a b
