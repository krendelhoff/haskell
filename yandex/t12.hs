isTriangle a b c = a + b > c && a + c > b && b + c > a

main = do
  [a, b, c] <- map read <$> sequence (replicate 3 getLine) :: IO [Int]
  putStrLn
    (if isTriangle a b c
       then "YES"
       else "NO")
