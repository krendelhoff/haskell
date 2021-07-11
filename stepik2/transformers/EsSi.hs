import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State

type EsSi = ExceptT String (State Integer)

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res =
        if odd n
          then 3 * n + 1
          else n `div` 2
  put res
  return n

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi m s = runState (runExceptT m) s

f a b x
  | x <= a = throwE "Lower bound"
  | x >= b = throwE "Upper bound"
  | otherwise = return ()

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go a b st = do
  x <- lift $ st >> st
  lift $ put x
  f a b x
{-
go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go lower upper next = do
  lift next -- инициализируем внутреннюю монаду
  n <- lift get
  when (n <= lower) (throwE "Lower bound")
  when (n >= upper) (throwE "Upper bound")
-}
