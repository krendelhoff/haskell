fib n = (\(a, _, _) -> a) $ until (\(_, _, c) -> c >= n) ss zz
  where
    zz = (0, 1, 0)
    ss (a, b, c) = (b, a + b, c + 1)
