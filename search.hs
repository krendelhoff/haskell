search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
   in foldl
        (\acc x ->
           if take nlen x == needle
             then True
             else acc)
        False
        (tails haystack)

-- используя tails и inits можно итерироватся по списку без explicit recursion используя folds
--
biba =
  let w = "w00t"
   in zip (inits w) (tails w)

-- == [("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
--
boba = findIndices (`elem` ['A' .. 'Z']) "Where Are The Caps?"
