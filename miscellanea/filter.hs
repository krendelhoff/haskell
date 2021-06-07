import Data.Char

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) 
  | p x       = x : filter p xs
  | otherwise = filter p xs

removePunctuation = filter (not . isPunctuation)
removeEmpty = filter (\x -> length x > 0)
