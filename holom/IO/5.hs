newtype O f g a =
  O
    { unO :: f (g a)
    }

instance (Functor f, Functor g) => Functor (O f g) where
  fmap h oVal = O $ fmap (fmap h) $ unO oVal

instance (Applicative f, Applicative g) => Applicative (O f g) where
  pure = O . pure . pure
  (O af) <*> (O ax) = O $ fmap (<*>) af <*> ax
