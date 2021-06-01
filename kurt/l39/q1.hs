import           Data.Char
import           Prelude   hiding (last, succ, tail)

addStrInts :: String -> String -> Either String Int
addStrInts s1 s2
  | not (res1 || res2) = Left "Neither value can be parsed"
  | not res1 = Left "First value can't be parsed"
  | not res2 = Left "Second value can't be parsed"
  | otherwise = Right $ read s1 + read s2
  where
    res1 = all isDigit s1
    res2 = all isDigit s2

--задание тупое, сильно лучше было бы вычислять в монаде Either
--
succ :: Int -> Maybe Int
succ x
  | x == maxBound = Nothing
  | otherwise = Just $ x + 1

tail :: [a] -> [a]
tail []     = []
tail (_:xs) = xs

maxElems = 50000

last :: [a] -> Either String a
last [] = Left "Empty list!"
last lst = listH lst 0
  where
    listH [x] _ = Right x
    listH (x:xs) n
      | n > maxElems = Left "List is too big!"
      | otherwise = listH xs (n + 1)
