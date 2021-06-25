{-# LANGUAGE TypeOperators #-}

infixr 9 |.|

newtype (|.|) f g a =
  Cmps
    { getCmps :: f (g a)
    }
  deriving (Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
-- Cmps f1 <*> Cmps f2 = Cmps $ liftA2 (<*>) f1 f2
  (Cmps af) <*> (Cmps ax) = Cmps $ fmap (<*>) af <*> ax
  -- по типам(и семантике) это пиздец очевидно
  -- fmap превращает обычную функцию в функцию между контейнерами
  -- app превращает завернутую в контекст обычную функцию в функцию между контейнерами(контекстами(монадическими значениями)))
