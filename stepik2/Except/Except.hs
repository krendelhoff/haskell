module Except where

import           Control.Applicative
import           Control.Monad

data Except e a =
  Except
    { runExcept :: Either e a
    }

data ListIndexError
  = ErrIndexTooLarge Int
  | ErrNegativeIndex
  deriving (Eq, Show)

except :: Either e a -> Except e a
except = Except

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f exc =
  except $
  case runExcept exc of
    Left e  -> Left $ f e
    Right x -> Right x

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where
  pure = return
  (<*>) = ap

instance Monad (Except e) where
  return = except . Right
  ma >>= f =
    case runExcept ma of
      Left x  -> except . Left $ x
      Right x -> f x

instance (Monoid e) => Alternative (Except e) where
  empty = mzero
  (<|>) = mplus

instance (Monoid e) => MonadPlus (Except e) where
  mzero = except . Left $ mempty
  ma `mplus` mb =
    except $
    case runExcept ma of
      Left e -> either (Left . mappend e) Right (runExcept mb)
      r      -> r

throwE :: e -> Except e a
throwE = except . Left

catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h =
  case runExcept m of
    Left e  -> h e
    Right a -> except $ Right a
