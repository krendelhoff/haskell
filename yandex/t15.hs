type R = Int

type M = Int

type Entrance = Int

type Floor = Int

type K1 = Int

type N = Int

type C = Int

type K2 = Int

calcOnFloor :: Floor -> M -> Entrance -> K2 -> Int
calcOnFloor n m p k2 =
  let res = (k2 - (p - 1) * m)
   in if res `mod` n == 0
        then res `div` n
        else res `div` n + 1

main = do
  [k1, m, k2, p2, n2] <- (map read . words) <$> getLine
  return ()
