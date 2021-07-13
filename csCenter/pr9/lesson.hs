import           Control.Monad (ap, guard)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = do
  x <- xs
  guard (p x)
  return x

replicate' :: Int -> a -> [a]
replicate' n x = do
  [1 .. n]
  return x

($>.) :: Monad m => m a -> b -> m b
xs $>. y = xs >> return y

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' h ma = ma >>= return . h

-- засчет return никакого эффекта не происходит
(*>.) :: Monad m => m a -> m b -> m b
(*>.) = (>>)

liftA2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftA2' f xs ys = (f <$> xs) `ap` ys

(<*>.) :: Monad m => m (a -> b) -> m a -> m b
(<*>.) = ap

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
amb >=> bmc = \a -> amb a >>= bmc

join :: Monad m => m (m a) -> m a
join mma = mma >>= id
