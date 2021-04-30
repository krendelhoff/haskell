sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = sumX x 0 0
                where
                    sumX :: Integer -> Integer -> Integer -> (Integer, Integer)
                    sumX 0 _ _ = (0, 1)
                    sumX x len summ
                        | x `div` 10 == 0    = (summ + (x `mod` 10), len + 1)
                        | otherwise = sumX (x `div` 10) (len + 1) (summ + (x `mod` 10))

main :: IO ()
main = print (sum'n'count 1234)
