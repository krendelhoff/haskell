import           Data.List

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = xs `sum2` ys `sum2` zs
  where
    sum2 [] bs         = bs
    sum2 as []         = as
    sum2 (a:as) (b:bs) = (a + b) : sum2 as bs

sum3' :: (Num a) => [a] -> [a] -> [a] -> [a]
sum3' a b c = map sum $ transpose [a, b, c]

main = print $ sum3 [1, 2, 3] [4, 5] [6]
