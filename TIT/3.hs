newtype T5 a =
  T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap :: (a -> b) -> ((a -> Int) -> Int) -> ((b -> Int) -> Int)
  fmap h (T5 f) = T5 $ \fb -> f $ fb . h
