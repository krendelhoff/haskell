u = Just 5

v = Just 7

k =
  (\x ->
     if x == 5
       then Nothing
       else return x)

(u `mplus` v) >>= k == Nothing

(u >>= k) `mplus` (v >>= k) == Just 7
