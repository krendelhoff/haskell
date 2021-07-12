{-# LANGUAGE InstanceSigs #-}

module ReaderT where

newtype ReaderT r m a =
  ReaderT
    { runReaderT :: r -> m a
    }

reader :: Monad m => (r -> a) -> ReaderT r m a
reader = ReaderT . (return .)

instance Functor m => Functor (ReaderT r m) where
  fmap h rdr = ReaderT $ fmap h . runReaderT rdr

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . (pure .) . const
  -- ReaderT . const . pure делает то же самое
  af <*> ax = ReaderT $ \r -> ($) <$> runReaderT af r <*> runReaderT ax r
  --- ну или просто runReaderT af r <*> runReaderT ax r лол
  -------------------- ReaderT $ liftA2 (<*>) (runReader f) (runReaderT ax)
  -- смысл - слезаем внутрь внешнего контекста и там аплликатируем
  -- это в точности одно и то же
  --

instance Monad m => Monad (ReaderT r m) where
  return = pure
  ma >>= f =
    ReaderT $ \r ->
      let bob = runReaderT m r
       in bob >>= (\a -> runReaderT (f a) r)
       -- m >>= k = Reader $ \r -> let v = runReader m r
       --                          in runReader (k v) r
       --      то же самое дословно
       --      только теперь мы выполняем еще один эффект
       --
       --
       --      ReaderT $ \env -> do    -- вычисление в абстрактной
       --               v <- runReaderT m r       внутренней монаде
       --               runReaderT (f a) r
       --

ask :: Monad m => ReaderT r m r
ask = asks id

asks :: Monad m => (r -> a) -> ReaderT r m a
asks = reader

local :: Monad m => (r -> r) -> Reader r m a -> Reader r m a
local f rdr = ReaderT (runReaderT rdr . f)
