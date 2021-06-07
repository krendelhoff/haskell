import Data.Char

clean :: String -> String
clean xs = [ toLower x | x <- xs, isLetter x || x == ' ' ]

concat :: [[a]] -> [a]
concat = foldr (++) []

concatListComprehension :: [[a]] -> [a]
concat xxs = [ x | xs <- xxs, x <- xs ]

-- концепт очень жесткий - что можно второй генератор прямо из первого генерить
-- завтра эти главы мы проглядываем и пересматриваем, за ночь в голову всё уложится! просто продолжай ботать, читать и решать.
