import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Writer

-- траверс не сработает - длина меняется, структура не воссоздается
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate _ _ [] = return []
separate p1 p2 (x:xs) = do
  when (p1 x) (tell [x])
  when (p2 x) (lift $ tell [x])
  xs <- separate p1 p2 xs
  if ((&&) <$> (not . p1) <*> (not . p2) $ x)
    then return (x : xs)
    else return xs

-- вот то что обобщает твою explicit recursion, от которой надо уходить
separate' p1 p2 =
  filterM
    (\x -> do
       when (p1 x) (tell [x])
       when (p2 x) (lift $ tell [x])
       return $ (&&) <$> (not . p1) <*> (not . p2) $ x)
