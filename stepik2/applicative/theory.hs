instance Applicative Maybe where
  pure = Just
  Nothing <*> _  = Nothing
  (Just g) <*> x = fmag g x
-- fmag g cont == pure g <*> cont
-- именно отсюда берется (+) <$> Just 2 <*> Just 3 вот такой прием постоянно применяющийся
-- pure (+) <*> Just 2 <*> Just 3 == fmap (+) (Just 2) <*> Just 3 == (+) <$> Just 2 <*> Just 3
--
-- u (*>) u = (flip const) <$> u <*> w
-- u (<*) u = const <$> u <*> w
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftA f a = pure f <*> a -- по закону Аппликативов это обязано быть идентичным fmap (<$>)
-- liftA2 f a b = f <$> a <*> b
-- liftA3 f a b c = f <$> a <*> b <*> c
--
-- (<**>) :: Applicative f => f a -> f (a -> b) -> f b
-- (<**>) = liftA2 (flip ($))
--
-- это НЕ ТО ЖЕ САМОЕ, что flip (<*>), т.к.
-- в первом случае порядок эффектов меняется(сначала f a, потом f (a -> b)), а во втором - не меняется
--
{- liftA2 f a b = f <$> a <*> b

flip f x y = f y x

($) f x = f x

(<*>) = liftA2 id

-- liftA2 (flip ($)) a b = flip ($) <$> a <*> b                   = (\x y -> y x) <$> a <*> b

-- flip   (<*>)      a b = flip (liftA2 id) a b = (liftA2 id) b a =            id <$> b <*> a
Не знаю станет ли понятнее так, но очевидно, что в конечном счёте, при одинаковых сигнатурах, вычисления в последней части происходят в разном порядке.-}
-- по сути в аппликативе, после распаковки функции из контекста ВСЕГДА по семантике происходит fmap - это понятно
