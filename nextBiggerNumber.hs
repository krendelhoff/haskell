import           Control.Monad.Writer
import           Data.List

nextBigger :: Int -> Int
nextBigger n =
  let (ret, res) = runWriter . tryFlip . reverse . show $ n
   in if res == "lost"
        then -1
        else read . reverse $ ret
  where
    tryFlip :: String -> Writer String String
    tryFlip [] = tell "lost" >> return []
    tryFlip [x] = tell "lost" >> return [x]
    tryFlip (x:y:xs) = do
      if y < x
        then tell "win" >> return (y : x : xs)
        else tryFlip (y : xs) >>= (\ys -> return (x : ys))

-- ты гений, монады прекрасны. Все эффекты идеально контроллируются.
main = print $ nextBigger (-521)
