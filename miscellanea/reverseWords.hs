reverseWords :: String -> String
reverseWords s = helper [] s
  where
    helper acc [] = acc
    helper acc l@(x:xs)
      | x == ' ' = helper (acc ++ [' ']) xs
      | otherwise = helper (acc ++ revWord) (drop count l)
      where
        revWord = reverse $ takeWhile (/= ' ') l
        count = length revWord

main :: IO ()
main = do
  print $ reverseWords "This is an example!"
