mapAsFold :: (a -> b) -> ([a] -> [b])
mapAsFold f = foldr (\x xs -> f x : xs) []

filterAsFold :: (a -> Bool) -> ([a] -> [a])
filterAsFold p = foldr (\x xs -> if p x then x : xs else xs) []

-- map and filter are folds actually
