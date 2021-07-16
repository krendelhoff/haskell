import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort array =
  runSTUArray $ do
    stArray <- thaw array
    let (a, b) = bounds array
    forM_ [(a + 1) .. b] $ \i -> do
      forM_ [a .. (b - i)] $ \j -> do
        x <- readArray stArray j
        y <- readArray stArray (j + 1)
        when (x > y) $ do
          writeArray stArray j y
          writeArray stArray (j + 1) x
    return stArray
