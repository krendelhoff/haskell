data Odd =
  Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd x) = Odd $ x + 2
  pred (Odd x) = Odd $ x - 2
  toEnum x = Odd (fromIntegral (1 + 2 * (x - 1)))
  fromEnum (Odd x) = (fromIntegral ((x - 1) `div` 2) + 1)
  enumFrom (Odd x) = (Odd x) : (enumFrom $ Odd (x + 2))
  enumFromThen (Odd x) (Odd y) =
    (Odd x) : (enumFromThen (Odd y) (Odd (2 * y - x)))
  enumFromTo (Odd x) (Odd y) =
    take (fromIntegral ((y - x) `div` 2) + 1) (enumFrom (Odd x))
  enumFromThenTo (Odd x) (Odd y) (Odd z) =
    take (fromIntegral count) $ enumFromThen (Odd x) (Odd y)
    where
      count = (z - x) `div` (y - x) + 1

main = do
  [raw1, raw2, raw3] <- sequence [readLn, readLn, readLn]
  raw4 <- readLn
  let [n1, n2, n3] = [Odd raw1, Odd raw2, Odd raw3]
      int = raw4
  print $ succ n1
  print $ pred n1
  print $ ((toEnum int) :: Odd)
  print $ take 10 $ enumFromThen n1 n2
  print $ enumFromTo n1 n2
  print $ enumFromThenTo n1 n2 n3
