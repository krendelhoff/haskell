import Data.Char

accum :: [Char] -> [Char]
accum s = accumHelper s 1 ""
        where
            len = length s
            accumHelper "" _ _ = ""
            accumHelper (x:str) n res
                | null str  = res ++ [toUpper x] ++ (replicate (n - 1) (toLower x))
                | otherwise = accumHelper str (n + 1) (res ++ [toUpper x] ++ (replicate (n - 1) (toLower x)) ++ "-")

main :: IO ()
main = print (accum "abcd")
