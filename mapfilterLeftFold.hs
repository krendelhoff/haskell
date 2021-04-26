mapAsLeftFold :: (a -> b) -> [a] -> [b]
mapAsLeftFold f = foldf (\xs x -> f x : xs) []

reverseFold :: [a] -> [a]
reverseFold = foldl (\xs x -> x : xs) []

foldReverse :: [a] -> [a]
foldReverse = foldl (flip (:)) []

sum' :: [a] -> Int
sum' = foldr (+) 0
-- потому что x : xs === flip (:) xs x === xs §:§ x

maxInt :: [Int] -> Int 
maxInt = foldl1 (\b a -> if a > b then a else b)

--   _ вот эти штуки важно ставить, т.к. хаскел не будет тогда вычислять лишний раз аргументы, т.к. это не нужно для паттерн матчинга. 
