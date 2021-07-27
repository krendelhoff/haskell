{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import           Fmt
import           Fmt.Internal.Formatters
import           System.Environment
import           System.Random

range = (1, 6)

playDice :: (MonadState (Int, Int) m, MonadIO m) => Int -> m ()
playDice threshold = do
  x <- liftIO $ randomRIO range
  liftIO $ fmtLn $ "First player got " +|| x |+ " score!"
  liftIO $ getLine
  modify $ bimap (+ x) id
  (s1, _) <- get
  if (s1 >= threshold)
    then liftIO $ putStrLn "First player won!"
    else do
      x <- liftIO $ randomRIO range
      liftIO $ fmtLn $ "Second player got " +|| x |+ " score!"
      liftIO $ getLine
      modify $ bimap id (+ x)
      (_, s2) <- get
      if (s2 >= threshold)
        then liftIO $ putStrLn "Second player won!"
        else playDice threshold

-- можно еще вывести промежуточные результаты
main = do
  [score] <- fmap read <$> getArgs
  (_, score) <- runStateT (playDice score) (0, 0)
  fmtLn $ "Total score " +| tupleF score
