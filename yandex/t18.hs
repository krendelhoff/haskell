import           Control.Monad

countMin a b n m = min nCase mCase
  where
    nCase = n + (n - 1) * a
    mCase = m + (m - 1) * b

main = do
  [a, b, n, m] <- map read <$> replicateM 4 getLine
  print $ countMin a b n m
