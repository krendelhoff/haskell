module Cont where

import           Control.Monad

newtype Cont r a =
  Cont
    { runCont :: (a -> r) -> r
    }

instance Functor (Cont r) where
  fmap = liftM

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r)
  -- return = Cont . (&)
  -- retrun = Cont . flip ($)
                              where
  return x = Cont $ \c -> c x
  ma >>= f = Cont $ \c -> runCont ma (\a -> runCont (f a) c)
  -- передаем функцию, которая воспользуется результатов вычисления
  -- это и есть continuation
  -- в этом суть CPS
  --
  --
  --

cont :: ((a -> r) -> r) -> Cont r a
cont = Cont

--square' 2 >>= (add' 3) >>= (add' 5) = \c -> c 4 >>= (\x c -> c $ add 3 x) >>= (\x c -> c $ add 5 x) =
evalCont :: Cont r r -> r
evalCont m = runCont m id

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = cont $ \c -> runCont (f (\a -> cont $ \_ -> c a)) c
 -- если k не вызовется, лямбда проигнорируется просто, и будет обычное продолжение, туда передается c (приведение к виду c $ x)
