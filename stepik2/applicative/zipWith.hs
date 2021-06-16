newtype ZipList a =
  ZipList
    { getZipList :: [a]
    }
  deriving (Ord, Eq, Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
  pure = ZipList . repeat
  (ZipList af) <*> (ZipList ax) = ZipList $ zipWith ($) af ax

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = getZipList $ f <$> ZipList xs

(>*<) :: [a -> b] -> [a] -> [b]
lf >*< xs = getZipList $ ZipList lf <*> ZipList xs

{-
instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right g <*> r = fmap g r
-}
-- очень логично и так же, как для Maybe
--(в fmap основная работа и разбор случаев уже прописаны,
--поэтому используем)
--
{- это по сути Writer
instance Monoid e => Applicative ((,) e) where
  pure x = (mempty, x)
  (u, g) <*> (v, x) = (u `mappend` v, g x)
-}
divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> ('<' : '-' : show x ++ "/", x) <*> (divideList' xs)
{-
instance Applicative ((->) e) where
  pure = const
--(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
  g <*> h = \e -> g e (h e)
-}
-- zip <*> tail $ [1,2,3] -- абсолютно логичная конструкция
-- zip [1,2,3] (tail [1,2,3]) -- хотим
