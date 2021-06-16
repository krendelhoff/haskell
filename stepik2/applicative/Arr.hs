newtype Arr2 e1 e2 a =
  Arr2
    { getArr2 :: e1 -> e2 -> a
    }

newtype Arr3 e1 e2 e3 a =
  Arr3
    { getArr3 :: e1 -> e2 -> e3 -> a
    }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 (\e1 e2 -> f $ g e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 (\e1 e2 e3 -> f $ g e1 e2 e3)

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \e1 e2 -> x
  (Arr2 af) <*> (Arr2 ax) = Arr2 $ \e1 e2 -> af e1 e2 (ax e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 $ \e1 e2 e3 -> x
  (Arr3 af) <*> (Arr3 ax) = Arr3 $ \e1 e2 e3 -> af e1 e2 e3 (ax e1 e2 e3)
