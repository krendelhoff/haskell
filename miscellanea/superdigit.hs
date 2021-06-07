fold [n, k] = [show $ foldr (\x acc -> read [x] + acc) 0 n, k]

superdigit n
  | n `div` 10 == 0 = n
superdigit n = superdigit (sumdig n)
  where
    sumdig 0 = 0
    sumdig n = (+) (n `mod` 10) $! sumdig (n `div` 10)

main = do
  rawLst <- getLine
  let [n, k] = map (read :: String -> Integer) . fold . words $ rawLst
      fsum = superdigit n
  print $ superdigit (fsum * k)
