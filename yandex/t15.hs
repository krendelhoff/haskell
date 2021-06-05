import           Text.Printf

type R = Int

type M = Int

type Entrance = Int

type Floor = Int

type K1 = Int

type N = Int

type C = Int

type K2 = Int

type Flats = Int

calcMinFloor :: Floor -> M -> Entrance -> K2 -> Int
calcMinFloor n m p k2 =
  let cac = (p - 1) * m + n
   in if k2 `mod` cac == 0
        then k2 `div` cac
        else k2 `div` cac + 1

calcEntranceFloor :: Flats -> M -> K1 -> (Entrance, Floor)
calcEntranceFloor flats m k1 = (ent, floor)
  where
    ent = (k1 `div` (flats * m)) + 1
    floor = (k1 `mod` (flats * m)) `div` flats + 1

-- рассмотреть еще случай k1 < k2
main = do
  [k1, m, k2, p2, n2] <- (map read . words) <$> getLine
  let minFloor = calcMinFloor n2 m p2 k2
      checkValid floor = ((p2 - 1) * minFloor * m + (n2 - 1) * floor) < k2
  if not . checkValid $ minFloor
    then printf "-1 -1\n"
    else if checkValid $ (minFloor + 1)
           then if m == 1
                  then printf "0 1\n"
                  else printf "0 0\n"
           else let (ent, floor) = calcEntranceFloor minFloor m k1
                 in printf "%d %d\n" ent floor
