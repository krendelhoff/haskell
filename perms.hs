perms :: [a] -> [[a]]
perms =
  foldl
    (\acc x -> concatMap (\perm -> insertEveryWhere (length perm) perm x) acc)
    [[]]
  where
    insert i lst x = (take i lst) ++ [x] ++ (drop i lst)
    insertEveryWhere len lst x =
      foldl (\acc i -> (insert i lst x) : acc) [] [0 .. len]

-- почти никогда не нужна explicit recursion, только игра с fold
-- видишь, тут то по чему фолдим казалось бы вообще к результату отношения не имеет, просто итерируемся
-- это способ итерироваться по списку
main = do
  lstRaw <- getLine
  let lst = read lstRaw :: [Int]
  print $ lst
  print $ perms lst
  print $ length lst
