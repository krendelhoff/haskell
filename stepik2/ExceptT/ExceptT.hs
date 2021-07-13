{-# LANGUAGE InstanceSigs #-}

module ExceptT where

import           Control.Applicative
import           Control.Monad.Trans

newtype ExceptT e m a =
  ExceptT
    { runExceptT :: m (Either e a)
    }

except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . return

instance Functor m => Functor (ExceptT e m) where
  fmap h = ExceptT . fmap (fmap h) . runExceptT
  -- Either уже Functor

instance Monad m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  mef <*> mea =
    ExceptT $ do
      ef <- runExceptT mef
      either (return . Left) (\f -> fmap (fmap f) runExceptT mea) ef
                            -- два барьера - m и Either
  --af <*> ax = ExceptT $ liftA2 (<*>) (runExceptT af) (runExceptT ax)
  -- в аппликативном вычислении структура четко задана - он не может прерваться
  -- если выпало исключение, он проведет все две ветки вычислений (как в композиции аплликативов)
  -- поэтому тут можно адекватно использовать только монадический constraint

instance Monad m => Monad (ExceptT e m) where
  (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
  m >>= k =
    ExceptT $ do
      a <- runExceptT m -- тут происходит эффект и передается в следующий
      either (return . Left) (runExceptT . k) a

instance MonadFail m => MonadFail (ExceptT e m) where
  fail = ExceptT . fail

throwE :: Monad m => e -> ExceptT e m a
throwE = except . Left

instance MonadTrans (ExceptT e) where
  lift :: Monad m => m a -> ExceptT e m a
  lift = ExceptT . (Right <$>)

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h =
  ExceptT $ do
    a <- runExceptT m
    either (runExceptT . h) (return . Right) a
