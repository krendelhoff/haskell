import           Data.Monoid
import           Prelude     hiding (Identity, Maybe (..), Monad, Reader, State,
                              Writer)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a =
  Identity a
  deriving (Show, Eq)

data Maybe a
  = Nothing
  | Just a
  deriving (Show, Eq)

data State s a =
  State
    { runState :: s -> (a, s)
    }

data Reader s a =
  Reader
    { runReader :: s -> a
    }

data Writer w a =
  Writer
    { runWriter :: (w, a)
    }

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f  = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State g) >>= f =
    State $ \s ->
      let (y, newState) = g s
          (State p) = f y
       in p newState

instance Monad (Reader s) where
  return x = Reader $ (\_ -> x)
  (Reader g) >>= f =
    Reader $ \x ->
      let (Reader m) = f (g x)
       in m x

instance Monoid w => Monad (Writer w) where
  return x = Writer (mempty, x)
  (Writer (s, v)) >>= f =
    Writer $
    let (Writer (newLog, y)) = f v
     in (s `mappend` newLog, y)
