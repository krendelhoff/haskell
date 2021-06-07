import Data.List.Split
import Data.Char

toLowerise :: String -> String
toLowerise "" = ""
toLowerise (x:s) = (toLower x):toLowerise s

capitalize :: String -> String
capitalize "" = ""
capitalize (x:s) = (toUpper x):(map toLower s)

joinHelper :: [String] -> String -> String
joinHelper xs s
        | null xs          = s
        | length xs == 1   = joinHelper (tail xs) (s ++ head xs)
        | otherwise        = joinHelper (tail xs) (s ++ head xs ++ " ")


join :: [String] -> String
join s = joinHelper s ""

getLists :: String -> String -> ([String], [String])
getLists s1 s2 = (map toLowerise (splitOn " " s1), splitOn " " s2)

clearLists :: ([String], [String]) -> [String]
clearLists (a, b) = [ if (low `elem` a) then low else (capitalize x) | x <- b, let low = toLowerise x]

capSentence :: String -> String
capSentence "" = ""
capSentence (x:s) = (toUpper x):s

titleCase :: String -> String -> String
titleCase minor title = capSentence (join (clearLists (getLists minor title)))

main :: IO ()
main = print (titleCase "a an the of" "a clash of KINGS")
