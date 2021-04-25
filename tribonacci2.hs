tribonacciNum :: Num a => a -> a -> a -> Int -> a
tribonacci _ _ c 0 = c
tribonacciNum a b c n = tribonacci b c (a + b + c) (n - 1)

tribonacciHelper :: 
