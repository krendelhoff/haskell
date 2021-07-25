{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as M

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = evalState (eval f n) M.empty
  where
    eval :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> State (Map a b) b
    eval f a = do
      map <- get
      maybe
        (case f a of
           Left i -> return i
           Right (l, g) ->
             fmap g .
             traverse
               (\a -> do
                  i <- eval f a
                  modify (M.insert a i)
                  return i) $
             l)
        return $
        M.lookup a map

factorial i
  | i == 0 = Left 1
  | otherwise = Right ([i - 1], (* i) . head)

fibonacci i
  | i < 2 = Left i
  | otherwise = Right ([i - 1, i - 2], sum)

coinchange (a, i)
  | a == 0 = Left 1
  | a < 0 || i == 0 = Left 0
  | otherwise = Right ([(a, i - 1), (a - coinlist !! (i - 1), i)], sum)

coinlist = [1, 3, 5, 10]

heigth (n, m)
  | m <= 0 || n <= 0 = Left 0
  | otherwise = Right ([(n, m - 1), (n - 1, m - 1)], (+ 1) . sum)

foo i
  | i <= 2 = Left 1
  | odd i = Right ([6 * i `div` 7, 2 * i `div` 3], sum)
  | otherwise = Right ([i - 1, i - 3], sum)
