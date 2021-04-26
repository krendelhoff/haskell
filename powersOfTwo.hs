powersOfTwo = iterate (* 2) 2

main :: IO ()
main = print (take 5 powersOfTwo)
