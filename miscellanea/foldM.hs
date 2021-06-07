import           Control.Monad

-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
-- то есть всё таки можно добавить к свертке контекст, это ровно то, чего ты хотел.
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

main = print $ foldM binSmalls 0 [1, 2, 3, 10, 9]
