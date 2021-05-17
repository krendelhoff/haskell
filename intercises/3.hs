import           Furry

-- Exercise 6
-- Relative Difficulty: 3
-- (use banana and/or unicorn)
furry' :: (Misty m) => (a -> b) -> m a -> m b
furry' f m = (\x -> unicorn $ f x) `banana` m

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean m = (\x -> unicorn `banana` x) `banana` m

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple m mf = (\f -> (unicorn . f) `banana` m) `banana` mf

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs f =
  let lst = map f xs
   in sausage lst

-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage []     = unicorn []
sausage (x:xs) = (\y -> (\t -> unicorn (y : t)) `banana` sausage xs) `banana` x

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = mb `apple` (f `furry'` ma)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = mc `apple` (mb `apple` (f `furry'` ma))

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 ::
     (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = md `apple` (mc `apple` (mb `apple` (f `furry'` ma)))
