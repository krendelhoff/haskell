fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n
  | n < 0 = fibNeg 0 1 n
  where
    fibNeg a _ 0 = a
    fibNeg a b n = fibNeg (b - a) a (n + 1)
fibonacci n = fibPos 0 1 (n - 1)
  where
    fibPos _ b 0 = b
    fibPos a b n = fibPos b (a + b) (n - 1)
