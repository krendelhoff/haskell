instance (Eq a) => Eq [a] where
  [] == []         = True
  (x:xs) == (y:ys) = all ((x == y) : (xs == ys))
