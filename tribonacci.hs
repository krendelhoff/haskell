import Data.Matrix

ntimes :: Num a => Matrix a -> Matrix a -> Int -> Matrix a
ntimes m res 0 = res
ntimes m res n = ntimes m (res * m) (n - 1)

tupleToMatrix :: Num a => (a, a, a) -> Matrix a
tupleToMatrix (a, b, c) = fromList 1 3 [a, b, c]

-- скобки важны! до этого он сначала запускал рекурсию, а только потом прибавлял getElem, что нарушало схостовость. Шаришь!

tribonacciHelper :: Num a => (a, a, a) -> Int -> Matrix a -> Int -> [a] -> [a]
tribonacciHelper (a, b, c) n matrix len list
        | (len <= 0)    = []
        | (n == len)    = list
        | otherwise     = tribonacciHelper (a, b, c) (n + 1) (matrix * matri) len (list ++ [getElem 1 3 (basic * matrix)])
        where basic = tupleToMatrix (a, b, c)
              matri = fromList 3 3 [0, 0, 1, 1, 0, 1, 0, 1, 1] :: Num a => Matrix a

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n
        | n <= 0        = []
        | n == 1        = [a]
        | n == 2        = [a, b]
        | otherwise     = a:b:tribonacciHelper (a, b, c) 0 (identity 3) (n - 2) []

n = 198
main :: IO ()
main = print (tribonacci (1,1,1) n)

--tribonacci :: Num a => (a, a, a) -> Int -> [a]
--tribonacci (a, _, _) 1 = a
--tribonacci (_, b, _) 2 = b
--tribonacci (_, _, c) 3 = c
--tribonacci (a, b, c) n = tribonacci
