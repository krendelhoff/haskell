-- show State instances for Fluffy and Misty
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

newtype State s a =
  State
    { state :: (s -> (s, a))
    }

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f fa =
    State $ \s ->
      let (s', a) = state fa s
       in (s', f a)

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana kl ma =
    State $
    (\s ->
       let (s', a) = state ma s
           (State g) = kl a
        in g s')
  unicorn a = State $ \s -> (s, a)

-- /show
main = putStrLn "It typechecks!"
