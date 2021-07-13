import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable

limited p fs = traverse limit1 (zip [0 ..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

run1 m s = runState (runExceptT m) s

run2 m s = runExcept (runStateT m s)

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s
