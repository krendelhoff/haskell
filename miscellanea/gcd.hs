import           Control.Monad.Writer

gcd' :: (Integral a, Show a) => a -> a -> Writer [String] a
gcd' a 0 = do
  tell $ ["Final result is: " ++ show a]
  return a
gcd' a b = do
  let r = a `mod` b
  tell $ [show a ++ " % " ++ show b ++ " = " ++ show r]
  gcd' b r

main = do
  n1 <- readLn
  n2 <- readLn
  mapM_ putStrLn . snd . runWriter $ gcd' n1 n2
