import           Data.Char

isValidIsbn :: String -> Bool
isValidIsbn s = isValidIsbnH (filter (/= '-') s)
  where
    isValidIsbnH :: String -> Bool
    isValidIsbnH s
      | not . all isDigit $ s = False
      | length s /= 10 = False
      | otherwise =
        if sum (zipWith (\x y -> (read [y] :: Int) * x) [10,9 .. 1] s) `mod` 11 /=
           0
          then False
          else True
