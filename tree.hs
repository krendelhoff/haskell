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

inOrder :: Tree a -> [a]
inOrder Nil                 = []
inOrder (Node x left right) = (inOrder left) ++ [x] ++ (inOrder right)

preOrder :: Tree a -> [a]
preOrder Nil                 = []
preOrder (Node x left right) = [x] ++ (preOrder left) ++ (preOrder right)

postOrder :: Tree a -> [a]
postOrder Nil                 = []
postOrder (Node x left right) = (postOrder left) ++ (postOrder right) ++ [x]

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil                 = Nil
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ z Nil                 = z
foldTree f z (Node x left right) = f x (foldTree f z left) (foldTree f z right)

foldElemTree a = foldTree check False
  where
    check x l r = x == a || l || r

foldInOrder = foldTree combine []
  where
    combine x l r = l ++ [x] ++ r

leaves :: Tree a -> [a]
leaves Nil                 = []
leaves (Node x Nil Nil)    = [x]
leaves (Node x left right) = (leaves left) ++ (leaves right)

-- below everything for BST
singleton :: a -> Tree a
singleton x = Node x Nil Nil

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Nil = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Nil = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

main :: IO ()
main = do
  print (mapTree (* 2) (Node 4 (Node (-2) Nil Nil) (Node 6 Nil Nil)))
  print
    (foldTree
       (\x l r -> 1 + l + r)
       0
       (Node 1 (Node 2 Nil Nil) (Node 3 Nil (Node (-2) Nil Nil))))
  print (leaves intTree)
