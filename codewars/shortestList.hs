shortestList :: [[a]] -> [a]
shortestList [] = []
shortestList xs = xs !! fun (zip [0 .. (len - 1)] xs)
  where
    len = length xs
    fun :: [(Int, [a])] -> Int
    fun xs =
      let flist = filter (\(n, l) -> null l) xs
       in if null flist
            then fun (map (\(n, x) -> (n, tail x)) xs)
            else fst . head $ flist
    -- think! applicatives, zipLista and t.d.
