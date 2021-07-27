{-# LANGUAGE MultiWayIf #-}

import           Control.Exception
import           Control.Monad
import           Control.Monad.Writer
import           GHC.Exception.Type
import           System.Environment
import           System.Random

guess :: MonadIO m => Int -> Int -> m ()
guess last answer = do
  x <- liftIO $ readLn
  let diff1 = abs (last - answer)
      diff2 = abs (x - answer)
      action =
        if | diff1 < diff2  -> liftIO $ putStrLn "Colder..."
           | diff1 == diff2 -> liftIO $putStrLn "Same temperature!"
           | otherwise      -> liftIO $putStrLn "Warmer!"
  if (x == answer)
    then (liftIO $ putStrLn "You win!")
    else action >> guess x answer

tryToParseArg :: MonadIO m => m ()
tryToParseArg = do
  [a, b] <- map read <$> getArgs
  if (a < b)
    then randomRIO (a, b) >>= guess b
    else putStrLn "a > b, man"

main = tryToParseArg `catch` (const $ putStrLn "Wrong args")
