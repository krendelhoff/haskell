count :: Int -> [Int] -> Int
count n =
  foldr
    (\x acc ->
       if x == n
         then 1 + acc
         else acc)
    0

doBobaLst [] k = []
doBobaLst l@(x:xs) k =
  let countElem = count x l
   in if countElem >= k
        then x : doBobaLst (filter (/= x) xs) k
        else doBobaLst (filter (/= x) xs) k

showLst []     = "-1"
showLst [x]    = show x
showLst (x:xs) = mconcat [show x, " ", showLst xs]

doBoba :: IO ()
doBoba = do
  rawNums <- getLine
  rawLst <- getLine
  let [n, k] = map (read :: String -> Int) . words $ rawNums
      lst = map (read :: String -> Int) . words $ rawLst
      printLst = doBobaLst lst k
  putStrLn $ showLst printLst

main = do
  testCases <- readLn
  sequence_ $ replicate testCases doBoba
