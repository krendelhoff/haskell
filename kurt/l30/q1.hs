allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f ma = ma >>= (\x -> return $ f x)

-- превращаем эту хуйню в стрелку клейсли и теперь это под юристикцией bind и Monad typeclass
--
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf ma = mf >>= (\f -> allFmapM f ma)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _  = Nothing
bind (Just x) f = f x
