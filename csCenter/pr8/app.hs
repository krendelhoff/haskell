{-
instance Applicative ((->) e) where
  pure = const
  -- просто записав тип это становится понятно
  f <*> g = \e -> f e (g e)
-}
-- это и весь функция многих аргументов для контейнера(контекста) ((->) e)
-- точнее функция многих аргументов завернутая в него, и значения(аргументы) завернуты
r :: (e -> a -> b -> c) -> (e -> a) -> (e -> b) -> e -> c
--r = \f g h -> f <*> g <*> h
r = \f g h e -> f e (g e) (h e)

example = (\a b c -> [a, b, c]) <$> (+ 5) <*> (* 3) <*> (/ 2) $ 7

-- суть - навешивание эффектов на аргументы
--
{-
instance Applicative (Either e) where
  pure = Right
  (Left x) <*> _ = Left x
  (Right f) <*> g = fmap f g
-}
newtype Cmps f g x =
  Cmps
    { getCmps :: f (g x)
    }

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

ffmap h = getCmps . fmap h . Cmps
