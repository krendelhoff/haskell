harvesineIO :: IO LatLong -> IO LatLong -> IO Double
harvesine l1 l2 = do
  v1 <- l1
  v2 <- l2
  return (harvesine v1 v2)

harvesineIO :: IO LatLong -> IO LatLong -> IO Double
harvesineIO l1 l2 = harvesine <$> l1 <*> l2
