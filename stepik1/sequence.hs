sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) = do
  x <- m
  xs <- sequence ms
  return (x : xs)

sequence :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (m:ms) = m >>= \x -> sequence ms >>= \xs -> return (x : xs)

sequence :: Monad m => [m a] -> m [a]
sequence =
  foldr
    (\m acc -> do
       x <- m
       xs <- acc
       return (x : xs))
    (return [])

sequence :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (m:ms) = (:) <$> m <*> sequence ms
