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
