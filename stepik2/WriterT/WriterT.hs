{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module WriterT where

import           Control.Monad.Trans

newtype WriterT w m a =
  WriterT
    { runWriterT :: m (a, w)
    }

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT

instance Functor m => Functor (WriterT w m) where
  fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
  fmap f = WriterT . fmap updater . runWriterT
          -- есть некоторое хитрые ситуации, где ни x, ни log не нужны, поэтому
          -- ленивый образец ( делание его неопровержимым до последнего момента)
    where
      updater ~(x, log) = (f x, log) -- полезно и повышает определенность
      -- это так наываемый lazy Writer, бывает и strict
      -- реализация отличается только на тильду
      --

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure = WriterT . pure . (, mempty)
  af <*> ax = WriterT $ updater <$> (runWriterT af) <*> (runWriterT ax)
    where
      updater ~(g, w) ~(x, w') = (g x, w `mappend` w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  ma >>= f =
    WriterT $ do
      ~(v, w) <- runWriterT ma
      ~(v', w') <- runWriterT (f v)
      return (v', w `mappend` w')

instance (MonadFail m, Monoid w) => MonadFail (WriterT w m) where
  fail :: String -> WriterT w m a
  fail = WriterT . fail

instance (Monoid w) => MonadTrans (WriterT w) where
  lift :: Monad m => m a -> WriterT w m a
  lift m = WriterT $ (, mempty) <$> m

tell :: Monad m => w -> WriterT w m ()
tell w = writer ((), w)

listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
listen m =
  WriterT $ do
    ~(a, w) <- runWriterT m
    return ((a, w), w)

censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f m =
  WriterT $ do
    ~(a, w) <- runWriterT m
    return (a, f w)
