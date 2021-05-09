calc :: (Int -> Int -> Int) -> Int -> Int -> Writer String Int
calc op arg1 arg2 =
  let res = arg1 `op` arg2
   in if abs res < 128
        then return res
        else tell "overflow" >>= (\x -> return res)
