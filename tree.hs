data Tree a
  = Nil
  | Node a (Tree a) (Tree a)
  deriving (Show)

intTree :: Tree Int
intTree =
  (Node
     4
     (Node 7 (Node 19 Nil Nil) Nil)
     (Node (-3) (Node 8 Nil Nil) (Node 12 Nil Nil)))

elemTree :: (Eq a) => a -> Tree a -> Bool
elemTree _ Nil = False
elemTree a (Node x left right)
  | a == x = True
  | otherwise = (elemTree a left) || (elemTree a right)
