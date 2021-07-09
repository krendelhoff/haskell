import           Control.Monad.Cont

bar :: Char -> String -> Cont r Int
bar c s = do
  msg <-
    callCC $ \k -> do
      let s0 = c : s
      when (s0 == "hello") $ k "They say hello."
      let s1 = show s0
      return ("They appear to be saying " ++ s1)
  return (length msg)
