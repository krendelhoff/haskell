import           Data.List

count x n = doCount x possible
  where
    possible =
      map (^ n) [1 .. (floor . (** (1 / fromIntegral n)) . fromIntegral $ x)]
    doCount x _
      | x < 0 = 0
    doCount 0 _ = 1
    doCount x lst =
      sum $ map (\(y, n) -> doCount (x - y) (drop n lst)) (zip lst [1 ..])

main = do
  [x, n] <- sequence [readLn, readLn]
  print $ count x n
