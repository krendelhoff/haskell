import           Control.Monad (ap, foldM, guard)

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
--amb >=> bmc = \a -> amb a >>= bmc
amb >=> bmc = \a -> join . fmap bmc $ amb a

join :: Monad m => m (m a) -> m a
join mma = mma >>= id

type Board = Int

next :: Board -> [Board]
next ini = filter (>= 0) . filter (<= 9) $ [ini + 2, ini - 1]

doNTurns :: Int -> Board -> [Board]
doNTurns n ini = foldr (>=>) return (replicate n next) ini
