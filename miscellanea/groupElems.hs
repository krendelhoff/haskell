groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = (x : a) : (groupElems b)
  where
    (a, b) = span (x ==) xs

main = print $ groupElems [1, 1, 1, 1, 1, 2]
