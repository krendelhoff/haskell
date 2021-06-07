import           Control.Monad

check :: Int -> Int -> Int -> Int -> Bool
check a b d e = a <= d && b <= e || b <= d && a <= e

main = do
  [a, b, c, d, e] <- map read <$> replicateM 5 getLine
  if check a b d e || check a c d e || check b c d e
    then putStrLn "YES"
    else putStrLn "NO"
