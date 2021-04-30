x |-| y = let diff = x - y in sign diff * diff
            where
                 sign x
                        | x < 0    = -1
                        | x == 0   = 0
                        | x > 0    = 1

main :: IO ()
main = print (5 |-| 7)
