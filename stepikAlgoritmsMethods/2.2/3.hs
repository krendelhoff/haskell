import           Control.Monad
import           Control.Monad.State

tick :: Int -> State (Int, Int) ()
tick m = do
  (a, b) <- get
  put (b, (a + b) `mod` m)

fibonacci :: Int -> Int -> Int
fibonacci _ 0 = 0
fibonacci m n = snd . execState (replicateM_ (n - 1) $ tick m) $ (0, 1)

main = do
  [n, m] <- map read . words <$> getLine
  print $ fibonacci m n
