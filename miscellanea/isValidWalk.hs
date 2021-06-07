isValidWalk :: [Char] -> Bool
isValidWalk walk
  | not checkLen = False
  | otherwise =
    if l == 10 && n == s && w == e
      then True
      else False
  where
    checkLen =
      if (length $ take 11 walk) > 10
        then False
        else True
    (n, s, w, e, l) =
      foldr
        (\x (n, s, w, e, l) ->
           case x of
             'n' -> (n + 1, s, w, e, l + 1)
             's' -> (n, s + 1, w, e, l + 1)
             'w' -> (n, s, w + 1, e, l + 1)
             'e' -> (n, s, w, e + 1, l + 1))
        (0, 0, 0, 0, 0)
        walk

-- ленивая природа языка позволила тебе сделать это - он и не будет вычислять этот беск. список, т.к. если длина больше, то он уже нигде не пригодится
main = print $ isValidWalk $ repeat 'n'
