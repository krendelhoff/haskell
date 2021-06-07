import Data.List.Split

songDecoder :: String -> String
songDecoder s = strJoin [ x | x <- splitOn "WUB" s, not $ null x] ""
                where
                    strJoin [] _ = ""
                    strJoin (x:xs) res
                        | null xs   = res ++ x
                        | otherwise = strJoin xs (res ++ x ++ " ")

main :: IO ()
main = print (songDecoder "WUBIWUBAMWUBDOTAWUB2")
