module Main where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.STRef

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_
    [0 .. end]
    (\i -> do
       let val = vals !! i
       writeArray myArray i val)
  return myArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

swapST :: (Int, Int) -> (Int, Int)
swapST (x, y) =
  runST $ do
    x' <- newSTRef x
    y' <- newSTRef y
    writeSTRef x' y
    writeSTRef y' x
    xfinal <- readSTRef x'
    yfinal <- readSTRef y'
    return (xfinal, yfinal)

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray =
  runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
      forM_ [0 .. (end - i)] $ \j -> do
        val <- readArray stArray j
        nextVal <- readArray stArray $ j + 1
        let outOfOrder = val > nextVal
        when outOfOrder $ do
          writeArray stArray j nextVal
          writeArray stArray (j + 1) val
    return stArray

crossover ::
     (UArray Int Bool, UArray Int Bool)
  -> Int
  -> (UArray Int Bool, UArray Int Bool)
crossover (r1, r2) cp =
  let end = (snd . bounds) r1
   in (partCrossOver r1 r2, partCrossOver r2 r1)
  where
    end = (snd . bounds) r1
    partCrossOver r1 r2 =
      runSTUArray
        (do st <- thaw r1
            forM_ [cp .. end] $ \i -> do
              let val = r2 ! i
              writeArray st i val
            return st)

replaceZeroes :: UArray Int Int -> UArray Int Int
replaceZeroes arr =
  runSTUArray $ do
    let (start, end) = bounds arr
    st <- thaw arr
    forM_ [start .. end] $ \i -> do
      val <- readArray st i
      when (val == 0) $ do writeArray st i (-1)
    return st

main :: IO ()
main = return ()
