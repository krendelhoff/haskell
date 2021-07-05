data OddC a
  = Un a
  | Bi a a (OddC a)
  deriving (Eq, Show)

instance Functor OddC where
  fmap h c = c >>= (return . h)

instance Foldable OddC where
  foldr f ini (Un a)        = f a ini
  foldr f ini (Bi x y boba) = f x $ f y $ foldr f ini boba

instance Traversable OddC where
  traverse f (Un a)        = Un <$> f a
  traverse f (Bi x y boba) = Bi <$> f x <*> f y <*> traverse f boba

instance Monad OddC where
  return = Un
  ma >>= f = concatOC $ fmap f ma

instance Applicative OddC where
  pure = return
  (<*>) = ap

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x y bib) c2 c3     = Bi x y (concat3OC bib c2 c3)
concat3OC (Un a) (Bi x y bib) c3 = Bi a x (concat3OC (Un y) bib c3)
concat3OC (Un a) (Un b) c3       = Bi a b c3

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un c)         = c
concatOC (Bi c1 c2 ccc) = concat3OC c1 c2 $ concatOC ccc
