naiveReverse :: String -> String
naiveReverse "" = ""
naiveReverse (x:xs) = naiveReverse xs ++ [x]

fastReverse :: String -> String
fastReverse s = fastReverseAcc [] xs
                where fastReverseAcc acc []     = acc
                      fastReverseAcc acc (x:xs) = fastReverseAcc (x:acc) xs


