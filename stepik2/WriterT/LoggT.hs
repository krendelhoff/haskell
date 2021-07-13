import           Control.Monad.Identity
import           Control.Monad.Trans

data Logged a =
  Logged String a
  deriving (Eq, Show)

newtype LoggT m a =
  LoggT
    { runLoggT :: m (Logged a)
    }

instance Functor m => Functor (LoggT m) where
  fmap h = LoggT . fmap updater . runLoggT
    where
      updater ~(Logged log x) = Logged log $ h x

instance Applicative m => Applicative (LoggT m) where
  pure = LoggT . pure . Logged ""
  af <*> ax = LoggT $ updater <$> (runLoggT af) <*> (runLoggT ax) -- liftA2
    where
      updater ~(Logged w f) ~(Logged w' v) = Logged (w ++ w') $ f v

instance Monad m => Monad (LoggT m) where
  return = LoggT . return . Logged ""
  ma >>= f =
    LoggT $ do
      ~(Logged s a) <- runLoggT ma
      ~(Logged s' b) <- runLoggT (f a)
      return $ Logged (s ++ s') b

instance MonadFail m => MonadFail (LoggT m) where
  fail = LoggT . fail

write2log :: Monad m => String -> LoggT m ()
write2log = LoggT . return . (flip Logged ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

instance MonadTrans LoggT where
  lift = LoggT . (Logged "" <$>)
