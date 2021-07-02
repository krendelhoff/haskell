{-# LANGUAGE TypeOperators #-}

infixr 9 |.|

newtype (f |.| g) a =
  Cmps
    { getCmps :: f (g a)
    }
  deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldr f ini (Cmps cont) = foldr (\x acc -> foldr f acc x) ini cont
  foldMap f = foldMap (foldMap f) . getCmps
