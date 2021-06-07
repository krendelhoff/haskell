findEvenIndex :: [Int] -> Int
findEvenIndex [] = -1
findEvenIndex [_] = 0
findEvenIndex arr = helper 1 arr
  where
    helper n list
      | sum (tail arr) == 0 = 0
      | n == len = -1
      | len > n + 1 =
        if (sum (take n list) == sum (drop (n + 1) list))
          then n
          else (helper (n + 1) list)
      | len == n + 1 =
        if (sum (take n list)) == 0
          then n
          else -1
    len = length arr

main :: IO ()
main = do
  print (findEvenIndex [1, 2, 3, 4, 3, 2, 1])
