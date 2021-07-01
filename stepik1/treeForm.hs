numberTree :: Tree () -> Tree Integer
numberTree t = evalState (nTM t) $ 1
  where
    tick :: State Integer Integer
    tick = do
      n <- get
      put $ n + 1
      return n
    nTM :: Tree () -> State Integer (Tree Integer)
    nTM (Leaf _) = do
      n <- tick
      return (Leaf n)
    nTM (Fork t1 _ t2) = do
      tI <- nTM t1
      n <- tick
      tII <- nTM t2
      return (Fork tI n tII)
