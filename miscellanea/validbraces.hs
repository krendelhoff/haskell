rightOne :: Char -> Bool
rightOne c = c `elem` [')', '}', ']']

leftOne :: Char -> Bool
leftOne c = c `elem` ['(', '{', '[']

match :: Char -> Char -> Bool
match '(' ')' = True
match '{' '}' = True
match '[' ']' = True
match _ _ = False

validBraces :: String -> Bool
validBraces xs = goThrough xs ""
                where
                    goThrough "" (x:stack) = False
                    goThrough "" [] = True
                    goThrough (x:xs) stack
                      | leftOne x = goThrough xs (x:stack)
                      | rightOne x && null stack = False
                      | rightOne x && match (head stack) x  = goThrough xs (tail stack)
                      | otherwise  = False

-- важно писать оптимизированный код, хвостовую рекурсию
-- тип функции пиши в последнюю очередь, может он вообще не нужен будет

main :: IO ()
main = print (validBraces "))")
