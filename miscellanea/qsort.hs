qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    let leftSorted = qsort [ y | y <- xs, y <= x]
        rightSorted = qsort [ y | y <- xs, y > x]
    in  leftSorted ++ [x] ++ rightSorted

main :: IO ()
main = print $ qsort $ replicate 5 128
