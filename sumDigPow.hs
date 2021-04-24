getList :: Int -> [Int] -> [Int]
getList n xs    | (n == 0)  = xs
                | otherwise = getList (n `div` 10) ((n `mod` 10):xs)

sumF :: [Int] -> Int -> Int -> Int
sumF xs n sum       | null xs       = sum
                    | otherwise     = sumF (tail xs) (n + 1) (sum + (head xs)^(n + 1))

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = [ x | x <- [a..b], sumF (getList x []) 0 0 == x ]

main :: IO ()
main = do
    print (sumDigPow 483 3847)
