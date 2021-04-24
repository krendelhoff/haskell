(***) :: String -> Int -> String
(***) _ 0 = ""
(***) s n = s ++ (s *** (n - 1))

main :: IO ()
main = print ("haha" *** 3)
