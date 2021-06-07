data Nat
  = Zero
  | Succ Nat
  deriving (Show, Eq, Ord)

data Tree a
  = Nil
  | Node (Tree a) a (Tree a)

-- ord наследуется правильно и это звучит максимально логично
-- Zero -> Succ Zero -> Succ (Succ Zero) -> ...
--
beside :: Nat -> Nat -> Bool
beside (Succ x) y
  | x == y = True
beside x (Succ y)
  | x == y = True
beside _ _ = False

beside2 :: Nat -> Nat -> Bool
beside2 (Succ (Succ x)) y
  | x == y = True
beside2 x (Succ (Succ y))
  | x == y = True
beside2 _ _ = False

add :: Nat -> Nat -> Nat
add x y
  | x > y = add y x
add Zero y = y
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult x y
  | x > y = mult y x
mult Zero y = y
mult (Succ x) y = add y $ mult x y

pow :: Nat -> Nat -> Nat
pow x Zero     = x
pow x (Succ y) = mult x $ pow x y

reverseTree :: Tree a -> Tree a
reverseTree Nil            = Nil
reverseTree (Node t1 t t2) = Node (reverseTree t2) t (reverseTree t1)

depth :: Tree a -> Integer
depth Nil            = 0
depth (Node t1 _ t2) = 1 + max (depth t1) (depth t2)

foldTree :: (a -> b -> b -> b) -> Tree a -> b -> b
foldTree _ Nil acc            = acc
foldTree f (Node t1 t t2) acc = f t (foldTree f t1) (foldTree f t2)

leaves :: Tree a -> [a]
leaves Nil              = []
leaves (Node Nil t Nil) = [t]
leaves (Node t1 t t2)   = leaves t1 ++ leaves t2
