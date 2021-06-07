tribonacciHelper :: Num a => (a, a, a) -> [a] -> Int -> Int -> [a]
tribonacciHelper (a, b, c) list n len
        | n == len      = list
        | otherwise     = tribonacciHelper (b, c, a + b + c) (list ++ [c]) (n + 1) len

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci _ 0 = []
tribonacci (a, _, _) 1 = [a]
tribonacci (a, b, c) 2 = [a, b]
tribonacci (a, b, c) n = a:b:tribonacciHelper (a, b, c) [] 0 (n - 2)

-- это то же самое, что ты делал с матрицами. Абсолютно. Конкретно в данном случае умножение матриц бессмысленно.

main :: IO ()
main = print (tribonacci (1, 1, 1) 5)
