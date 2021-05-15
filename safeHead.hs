safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

showHead :: (Show a) => [a] -> String
showHead xs =
  case safeHead xs of
    Nothing -> "Empty list!"
    Just x  -> show x

safeHead' :: [a] -> Maybe a
safeHead =
  null >>=
  (\bool ->
     if bool
       then return Nothing
       else head >>= (\h -> return $ Just h))

-- то есть сюда сразу подается список, и результат применения мы подаем дальше
-- всё понятно
-- Just обязателен, т.к. используешь бесточечный стиль и на выходе должна быть функция
-- то есть тут реально идет считывание с контекста и результат монадического вычисления всегда зависит от него
main :: IO ()
main = do
  let empty = [] :: [Int]
  putStrLn (showHead [1 .. 5])
  putStrLn (showHead empty)
