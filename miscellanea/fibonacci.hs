fibHelper :: Integer -> Integer -> Int -> Integer
fibHelper _ larger 0 = larger
fibHelper smaller larger steps = fibHelper larger (smaller + larger) (steps - 1)

fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci n = fibHelper 0 1 (n - 1)

main :: IO ()
main = print (fibonacci 34899)
