{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Writer
import           Fmt
import           Fmt.Internal.Formatters

roots' :: (Num a, Ord a, Show a) => a -> (a -> a) -> a -> Writer [(a, a)] a
roots' eps f x0 = do
  let a = x0
      b = f x0
      diff = abs (a - b)
  tell [(a, diff)]
  if (diff <= eps)
    then return a
    else roots' eps f (f x0)

roots'' :: (Num a, Ord a, Buildable a, MonadIO m) => a -> (a -> a) -> a -> m ()
roots'' eps f x0 = do
  let a = x0
      b = f x0
      diff = abs (a - b)
  liftIO $ fmtLn $ build (a, diff)
  when (diff > eps) (roots'' eps f (f x0))

instance (Buildable a, Buildable b) => Buildable ((,) a b) where
  build = tupleF

main :: IO ()
main =
  fmtLn $
  nameF "Sequence of the approximations" $
  indentF 1 $
  blockListF $ execWriter $ roots' (0.001 :: Double) (\x -> x * x / 2) 0.5
