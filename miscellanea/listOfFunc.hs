listOfFunc = map (*) [0 ..]

main :: IO ()
main = print $ listOfFunc !! 4 $ 5
-- [(0*), (1*), (2*), ...]
