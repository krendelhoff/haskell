ntimes :: (String -> String) -> Int -> (String -> String)
ntimes f 1 = f
ntimes f n = f . (ntimes f (n - 1))

makePerm :: ([Int], [Int]) -> [Int] -> Int -> Int -> ([Int], [Int])
makePerm (s1, s2) inits n len   | (n == len)       = (s1, s2)
                                 | (n `mod` 2 == 1) = makePerm (s1 ++ [inits!!n], s2) inits (n + 1) len
                                 |(n `mod` 2 == 0)  = makePerm (s1, s2 ++ [inits!!n]) inits (n + 1) len

perm :: Int -> [Int]
perm 0 = []
perm len = fst tup ++ snd tup
        where
            tup = makePerm ([], []) [0..(len - 1)] 0 len

revPerm :: Int -> [Int]
revPerm 0 = []
revPerm len = (mergeList . splitList) [0..(len - 1)]

splitList :: [Int] -> ([Int], [Int])
splitList list = (take num list, drop num list)
                where
                    num = (length list) `div` 2

mergeListHelper :: ([Int], [Int]) -> Int -> [Int] -> [Int]
mergeListHelper (l1, l2) n res | null l1 && null l2 = res
  | (n `mod` 2 == 0) = mergeListHelper (l1, tail l2) (n + 1) (res ++ [head l2])
  | otherwise = mergeListHelper (tail l1, l2) (n + 1) (res ++ [head l1])

mergeList :: ([Int], [Int]) -> [Int]
mergeList (l1, l2) = mergeListHelper (l1, l2) 0 []

doPermHelper :: String -> String -> [Int] -> String
doPermHelper arg res perm | null perm  = res
                    | otherwise = doPermHelper arg (res ++ [arg!!(head perm)]) (tail perm)

doPerm :: String -> String
doPerm s = doPermHelper s "" (perm (length s))

doRevPerm :: String -> String
doRevPerm s = doPermHelper s "" (revPerm (length s))

encrypt :: String -> Int -> String
encrypt s n  | (n <= 0)  =  s
             | otherwise = (ntimes doPerm n) s

decrypt :: String -> Int -> String
decrypt s n  | (n <= 0)  =  s
             | otherwise = (ntimes doRevPerm n) s

-- Можно реализовать лучше! У тебя две идентичные фунции, будет настроение - реализуй полиморфизм

s = "hskt svr neetn!Ti aai eyitrsig"
n = 9
main :: IO ()
main = do
    print (decrypt s 1)
