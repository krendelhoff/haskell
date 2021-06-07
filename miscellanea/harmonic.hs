harmonic n =
  foldr
    (\x acc ->
       if x == n + 1
         then 0
         else (1 / fromIntegral x) + acc)
    0
    [1 ..]

isPalindrom x =
  let newx = map toLower $ filter (not . isSpace) x
   in newx == reverse newx
isPalindrom x =
  (\newx -> newx == reverse newx) (map toLower $ filter (not . isSpace) x)
