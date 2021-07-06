(***>) :: Applicative f => f a -> f b -> f b
ax ***> ay = flip const <$> ax <*> ay
