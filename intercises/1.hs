-- show Fluffy instances
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just x) = Just $ f x
  furry f Nothing  = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f = (f .)

newtype EitherLeft b a =
  EitherLeft (Either a b)

newtype EitherRight a b =
  EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a))  = EitherLeft $ Left $ f a
  furry f (EitherLeft (Right a)) = EitherLeft $ Right a

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight value) = EitherRight $ fmap f value

-- /show
main = putStrLn "It typechecks!"
