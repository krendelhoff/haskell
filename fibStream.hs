fibStream :: [Integer]
fibStream = 0 : 1 : (zipWith (+) fibStream (tail fibStream))

main = print $ take 10 fibStream
