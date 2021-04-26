any' :: [Bool] -> Bool
any' = foldr (||) False

elem' :: Eq a => a -> [a] -> Bool
elem' a xs = any (map (\x -> x == a) xs)

elemA'' :: Eq a => a -> [a] -> Bool
elemA'' a = any . (map (== a))
-- section of operator
