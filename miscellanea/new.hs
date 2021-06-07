seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqAHelper (n - 2) 1 2 3
        where
            seqAHelper :: Integer -> Integer -> Integer -> Integer -> Integer
            seqAHelper 0 _ _ c = c
            seqAHelper n a b c = seqAHelper (n - 1) b c (b + c - 2 * a)

main :: IO ()
main = print (seqA 301)
