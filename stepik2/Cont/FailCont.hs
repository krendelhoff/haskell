import           Control.Monad
import           Except
import           TryRead

newtype FailCont r e a =
  FailCont
    { runFailCont :: (a -> r) -> (e -> r) -> r
    }

instance Monad (FailCont r e) where
  return x = FailCont $ \ok _ -> ok x
  ma >>= f =
    FailCont $ \ok notOk ->
      runFailCont ma (\a -> runFailCont (f a) ok notOk) (\e -> notOk e)
      -- если вычисление успешно, передаем в дальнейшее (эта передача ok notOk есть просто "замыкание", внутри приводится к виду c $ x
      -- а если не успешно, просто передаем ошибку в продолжение notOk
      -- саспендим его
      -- ничего кроме этого сделать в принципе нельзя, т.к. вычисление выдано ошиьбку, а не значение, и продолжать его никак нельзя, можно только обработать
      -- по семантике всё понятно, это единственная разумная реализация

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

failturn :: e -> FailCont r e a
failturn e = FailCont $ \_ notOk -> notOk e

toFailCont :: Except e a -> FailCont r e a
toFailCont = either failturn return . runExcept

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fc = runFailCont fc Right Left

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f =
  FailCont $ \ok notOk ->
    runFailCont (f (\a -> FailCont $ \_ _ -> ok a)) ok notOk
