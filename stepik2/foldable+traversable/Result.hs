data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

instance Functor Result where
  fmap g (Ok x)    = Ok $ g x
  fmap g (Error s) = Error s

instance Foldable Result where
  foldr f ini (Ok a) = f a ini
  foldr f ini _      = ini

instance Traversable Result where
  traverse f (Ok a)       = Ok $ f a
  traverse _ (Error boob) = pure $ Error boob
