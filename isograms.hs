isIsogramHelper :: String -> String -> Bool
isIsogramHelper "" _ = True
isIsogramHelper (x:s) ss = if x `elem` ss then False else isIsogramHelper s (x:ss)

isIsogram :: String -> Bool
isIsogram s = isIsogramHelper s ""

main :: IO ()
main = print (isIsogram "Dermatoglyphics")
