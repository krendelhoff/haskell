import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

type RiiEsSiT m = ReaderT (Integer, Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT ::
     RiiEsSiT m a
  -> (Integer, Integer)
  -> Integer
  -> m (Either String a, Integer)
runRiiEsSiT m p s = runStateT (runExceptT $ runReaderT m p) s

tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
  n <- get
  let res =
        if odd n
          then 3 * n + 1
          else n `div` 2
  lift $ putStrLn $ show res
  put res
  return n

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go next = do
  lift . lift $ next
  n <- lift . lift $ get
  (a, b) <- ask
  when (n <= a) $ lift $ throwE "Lower bound"
  when (n >= b) $ lift $ throwE "Upper bound"
