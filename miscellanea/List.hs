data List a = Empty | NonEmpty a (List a) deriving (Show)
-- [3, 2, 6] :: [Int] === NonEmtry 3 $ NonEmpty 2 $ NonEmpty 6 Empty
-- just like 1 : 2 : 3 : []
-- причина того что такая запись работает - правая ассоциативность
--
elemList :: (Eq a) => a ->List a ->Bool
elemList _ Empty = False
elemList a (NonEmpty x xs)
  | x == a     = True
  | otherwise = elemList a xs

main = do
    print (elemList 1 Empty)
    print (elemList 3 (NonEmpty 2 (NonEmpty 3 Empty)))
