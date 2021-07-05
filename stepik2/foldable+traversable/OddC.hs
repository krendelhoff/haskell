data OddC a
  = Un a
  | Bi a a (OddC a)
  deriving (Eq, Show)

instance Functor OddC where
  fmap h (Un a)        = h a
  fmap h (Bi x y boba) = Bi (h x) (h y) (fmap h boba)

instance Foldable OddC where
  foldr f ini (Un a)        = f a ini
  foldr f ini (Bi x y boba) = f x $ f y $ foldr f ini boba

instance Traversable OddC where
  traverse f (Un a)        = Un <$> f a
  traverse f (Bi x y boba) = Bi <$> f x <*> f y <*> traverse f boba
