data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap h Nil              = Nil
  fmap h (Branch t1 x t2) = Branch (fmap h t1) (h x) (fmap h t2)

instance Foldable Tree where
  foldr f ini Nil              = ini
  foldr f ini (Branch t1 x t2) = foldr f (f x (foldr f ini t2)) t1

instance Traversable Tree where
  traverse f Nil = pure Nil
  traverse f (Branch t1 x t2) =
    Branch <$> (traverse f t1) <*> (f x) <*> (traverse f t2)
