import           Control.Monad
import           Control.Monad.State

tick :: State (Int, Int) ()
tick = do
  (a, b) <- get
  put (b, a + b)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci n = snd . execState (replicateM_ (n - 1) tick) $ (0, 1)

main = readLn >>= print . fibonacci
