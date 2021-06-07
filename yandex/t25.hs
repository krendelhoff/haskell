import           Control.Monad

format :: [Int] -> String
format []     = ""
format (x:xs) = mconcat [show x, " ", format xs]

add :: [Int] -> Int -> (Int, [Int])
add [] _ = (0, [])
add lst size =
  let rev = reverse lst
      dropQuan = addH rev size
   in (size - dropQuan, drop dropQuan rev)
  where
    isSym lst = lst == reverse lst
    addH rev 1 = 1
    addH rev n =
      if isSym rev
        then n
        else addH (init rev) (n - 1)

main = do
  size <- read <$> getLine
  seq <- (map read . words) <$> getLine
  let (len, boba) = add seq size
  print $ len
  when (not . null $ boba) (putStrLn $ format boba)
