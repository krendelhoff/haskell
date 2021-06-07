facs :: (Num a, Enum a) => [a]
facs = scanl (*) 1 [1 ..]

main = print $ take 12 facs
