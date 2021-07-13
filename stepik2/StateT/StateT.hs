{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module StateT where

import           Control.Monad.Trans (MonadTrans, lift)

newtype StateT s m a =
  StateT
    { runStateT :: s -> m (a, s)
    }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ return . f

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m = fmap snd . runStateT m

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m = fmap fst . runStateT m

get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

sl3 = StateT $ \st -> [(st + 1, 42), (st + 2, st), (st + 3, st * 2)]

prikol =
  runStateT
    (do sl3
        y <- get
        put (y - 2)
        return (2 * y))

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \s -> fmap updater $ runStateT m s
    where
      updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> return (x, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  af <*> ax =
    StateT $ \s -> do
      ~(g, s') <- runStateT af s
      ~(x, s'') <- runStateT ax s'
      return (g x, s'')
  -- а вот и прикол - аппликативного констрейнта недостаточно!
  -- оно и понятно! мы используем результат предыдущего вычисления, он влияет
  -- на последующее - передачей состояния
  -- то есть это можно сделать только монадическим интерфейсом
  -- аппликативный интерфейс имеет жесткую структуру вычислений и предыдущее
  -- не может влиять на следующее
  -- только в монадическом интерфейсе можно вынуть значение и передать его во второе и менять поведение
  --

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k =
    StateT $ \s -> do
      ~(x, s') <- runStateT m s
      runStateT (k x) s'

instance MonadFail m => MonadFail (StateT s m) where
  fail = StateT . const . fail

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> (, s) <$> m
  -- везде одна и та же суть - эффект во внешней монаде ТРИВИАЛЬНЫЙ
  -- так же, как у return
  -- StateT $ \st -> do
  --    a <- m
  --    return (a,st) -- это и есть fmap через do нотацию
