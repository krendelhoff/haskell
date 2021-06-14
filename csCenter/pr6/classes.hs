import           Prelude hiding (readList)

data Tree a
  = Leaf a
  | Branch (Tree a) a (Tree a)

instance (Eq a) => Eq (Tree a) where
  (Leaf a) == (Leaf b)                 = a == b
  (Branch t1 a t2) == (Branch t3 b t4) = t1 == t3 && a == b && t2 == t4
  _ == _                               = False

elemTree :: (Eq a) => a -> Tree a -> Bool
elemTree a (Leaf b)         = a == b
elemTree a (Branch t1 b t2) = a `elemTree` t1 || a `elemTree` t2 || a == b

instance Functor Tree where
  fmap f (Leaf a)         = Leaf $ f a
  fmap f (Branch t1 a t2) = Branch (fmap f t1) (f a) (fmap f t2)

data List a
  = Nil
  | Cons a (List a)

instance (Show a) => Show (List a) where
  show Nil          = "|>>>"
  show (Cons a lst) = ('<' : show a) ++ show lst

{-instance (Show a) => Show (Tree a) where
  show (Leaf a) = show a
  show (Branch t1 a t2) =
    ('<' : show t1) ++ "{" ++ show a ++ "}" ++ show t2 ++ ">"-}
instance (Show a) => Show (Tree a) where
  showsPrec _ (Leaf a) = (show a ++)
  showsPrec _ (Branch t1 a t2) =
    ('<' :) .
    showsPrec 0 t1 . ('{' :) . shows a . ('}' :) . showsPrec 0 t2 . ('>' :)

myReadsList :: (Read a) => ReadS (List a)
myReadsList ('|':s) = [(Nil, s)]
myReadsList ('<':s) =
  [(Cons x l, u) | (x, t) <- reads s, (l, '>':u) <- myReadsList t] -- бред

newtype Matrix a =
  Matrix [[a]]

instance (Show a) => Show (Matrix a) where
  showsPrec _ (Matrix [])     = ("EMPTY" ++)
  showsPrec _ (Matrix [x])    = shows x
  showsPrec _ (Matrix (x:xs)) = shows x . ('\n' :) . showsPrec 0 (Matrix xs)

class (Eq a, Bounded a, Enum a) =>
      SafeEnum a
  where
  ssucc :: a -> a
  spred :: a -> a
  ssucc a =
    if a == maxBound
      then minBound
      else succ a
  spred a =
    if a == minBound
      then maxBound
      else pred a

instance SafeEnum Bool

instance SafeEnum Int

rotate :: Int -> [a] -> [a]
rotate n xs =
  foldr
    (.)
    id
    (replicate
       (abs n)
       (if signum n >= 0
          then rotateLeft
          else rotateRight)) $
  xs
  where
    rotateLeft []     = []
    rotateLeft (x:xs) = xs ++ [x]
    rotateRight []  = []
    rotateRight lst = last lst : (init lst)

combs :: Int -> [a] -> [[a]]
combs n xs = map (map fst) $ comb n xs
  where
    comb n lst
      | n > length lst = []
    comb n _
      | n < 0 = []
    comb 0 _ = [[]]
    comb 1 xs = [[x] | x <- (zip xs [1 .. length xs])]
    comb n xs = concat [nextOrder comB | comB <- comb (n - 1) xs]
      where
        indexedList = zip xs [1 .. length xs]
        nextOrder combination =
          let (elem, index) = last combination
           in map (\x -> combination ++ [x]) (drop index indexedList)

perms :: [a] -> [[a]]
perms =
  foldr
    (\x acc -> concatMap (\perm -> insertEveryWhere (length perm) perm x) acc)
    [[]]
  where
    insert i lst x = (take i lst) ++ [x] ++ (drop i lst)
    insertEveryWhere len lst x =
      foldr (\i acc -> (insert i lst x) : acc) [] [0 .. len]
