{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Monoid

data Tree a
  = Leaf a
  | Fork (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap h (Leaf a)       = Leaf $ h a
  fmap h (Fork t1 x t2) = Fork (fmap h t1) (h x) (fmap h t2)

instance Foldable Tree where
  foldr f ini (Leaf a)       = f a ini
  foldr f ini (Fork t1 x t2) = foldr f (f x (foldr f ini t2)) t1

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Fork t1 x t2) =
    Fork <$> (traverse f t1) <*> (f x) <*> (traverse f t2)

data ReadError
  = EmptyInput
  | NoParse String
  deriving (Show)

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead "" = throwError EmptyInput
tryRead s =
  case reads s of
    ((a, ""):_) -> return a
    otherwise   -> throwError $ NoParse s

traverse_ f = sequence_ . fmap f

{-
treeSum :: Tree String -> Either ReadError Integer
treeSum t =
  fst . runWriter . runExceptT $ do
    (_, w) <- listen $ traverse_ go t
    return (getSum w)
  where
    go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
    go s = do
      x <- tryRead s
      tell (Sum x)
-}
-- Either тоже MonadError! traverse == mapM
treeSum :: Tree String -> Either ReadError Integer
treeSum = (sum <$>) . traverse tryRead
