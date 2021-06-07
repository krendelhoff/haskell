import           Data.List

revRange :: (Char, Char) -> [Char]
revRange (a, b) = unfoldr g b
  where
    g c =
      if a > c
        then Nothing
        else (Just (c, pred c))

main = print $ revRange ('a', 'z')
