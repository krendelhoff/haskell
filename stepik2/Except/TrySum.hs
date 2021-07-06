module TrySum where

import           Except
import           TryRead

data SumError =
  SumError Int ReadError
  deriving (Show)

trySum :: [String] -> Except SumError Integer
trySum =
  snd .
  foldl
    (\(n, acc) x ->
       ( succ n
       , do summ <- acc
            res <- withExcept (SumError n) (tryRead x)
            return $ summ + res))
    (1, return 0)

trySum' :: [String] -> Except SumError Integer
trySum' xs =
  sum <$>
  traverse (\(i, s) -> withExcept (SumError i) $ tryRead s) (zip [1 ..] xs)
