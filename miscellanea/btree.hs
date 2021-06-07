data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

tree = Node (Node (Leaf 7) (Leaf 8)) (Node (Node (Leaf 9) (Leaf 8)) (Leaf 128))

height :: Tree a -> Int
height (Leaf a)     = 0
height (Node t1 t2) = 1 + max (height t1) (height t2)

size :: Tree a -> Int
size (Leaf _)     = 1
size (Node t1 t2) = 1 + size t1 + size t2

avg :: Tree Int -> Int
avg t =
  let (c, s) = go t
   in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go (Leaf x)     = (1, x)
    go (Node t1 t2) = (go t1) <+++> (go t2)
    (<+++>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
    (x1, y1) <+++> (x2, y2) = (x1 + x2, y1 + y2)

main = print $ avg tree
