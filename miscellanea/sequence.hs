import           Prelude hiding (sequence)

sequence :: (Monad m) => [m a] -> m [a]
sequence = sequenceDo []
  where
    sequenceH acc []     = return acc
    sequenceH acc (x:xs) = x >>= (\y -> sequenceH (y : acc) xs)
    sequenceDo acc [] = return acc
    sequenceDo acc (x:xs) = do
      y <- x
      sequenceDo (y : acc) xs
