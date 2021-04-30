concat :: [[a]] -> [a]
concat list = [x | xs <- list, x <- xs]
