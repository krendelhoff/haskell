sequence :: (Monad m) => [m a] -> m [a]
sequence [] = Nothing
