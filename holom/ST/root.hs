import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST
import           Data.Array.ST
import           Data.STRef

limit = 0.0000000001

root :: (Double -> Double) -> Double -> Double -> Except String Double
root f a b = do
  when (f a * f b > 0) $ throwError "Invalid segment"
  return $
    runST $ do
      la <- newSTRef a
      lb <- newSTRef b
      iter f la lb
      readSTRef la
  where
    iter f la lb = do
      a <- readSTRef la
      b <- readSTRef lb
      let s = (a + b) / 2
      let diff = abs (a - b)
      if (f s == 0 || diff <= limit)
        then writeSTRef la s
        else if (f a * f s < 0)
               then writeSTRef lb s >> iter f la lb
               else writeSTRef la s >> iter f la lb
