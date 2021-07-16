import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.MArray
import           Data.Array.ST
import           Loops

swapElems :: Ix i => i -> i -> STArray s i e -> ST s ()
swapElems i j arr = do
  vi <- readArray arr i
  vj <- readArray arr j
  writeArray arr i vj
  writeArray arr j vi

qSort :: Ord a => [a] -> [a]
qSort xs =
  elems $
  runSTArray $ do
    arr <- newListArray (left, right) xs
    qsortST left right arr
    return arr
  where
    left = 0
    right = length xs - 1

qsortST :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
qsortST left right arr = do
  when (left <= right) $ do
    swapElems left ((left + right) `div` 2) arr
    vLeft <- readArray arr left
    (last, _) <-
      forLoop (left + 1) (<= right) succ (update vLeft) (return (left, arr))
    swapElems left last arr
    qsortST left (last - 1) arr
    qsortST (last + 1) right arr
  where
    update vLeft i st = do
      (last, arr) <- st
      vi <- readArray arr i
      if (vi < vLeft)
        then do
          swapElems (succ last) i arr
          return (succ last, arr)
        else do
          return (last, arr)
