isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) =
  if x < y
    then isSorted (y : xs)
    else False

main = do
  lst <- (map read . words) <$> getLine :: IO [Int]
  if isSorted lst
    then putStrLn "YES"
    else putStrLn "NO"
