module Furry where

-- show Misty instances
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f xs = concat (map f xs)
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing  = Nothing
  banana f (Just x) = f x
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g = (\t -> f (g t) t)
  unicorn x = (\_ -> x)

newtype EitherLeft b a =
  EitherLeft (Either a b)

newtype EitherRight a b =
  EitherRight (Either a b)

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x))  = f x
  banana f (EitherLeft (Right y)) = EitherLeft $ Right y
  unicorn x = EitherLeft $ Left x

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Left x))  = EitherRight $ Left x
  banana f (EitherRight (Right x)) = f x
  unicorn x = EitherRight $ Right x

-- /show
main = putStrLn "It typechecks!"
