import Data.List.Split
import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (x:s) = toUpper x:map toLower s

generateHashtag :: String -> String
generateHashtag "" = "false"
generateHashtag s  = if length result < 140 then result else "false"
                     where
                        result = "#" ++ glue (map capitalize (clean list))
                        list = splitOn " " s
                        clean list = [ x | x <- list, not $ null x]
                        glue [] = ""
                        glue list = (head list) ++ glue (tail list)

-- Не хвостовая рекурсия!
-- но красиво...

main :: IO ()
main = print (generateHashtag "hello world")
