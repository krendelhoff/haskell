type Mode = String

type Temperature = Int

type Time = Int

getTime :: Mode -> Temperature -> Temperature -> Time
getTime "freeze" t1 t2 = min t1 t2
getTime "heat" t1 t2   = max t1 t2
getTime "auto" _ t2    = t2
getTime "fan" t1 _     = t1

main = do
  [t1, t2] <- (map read . words) <$> getLine
  r <- getLine
  print $ getTime r t1 t2
