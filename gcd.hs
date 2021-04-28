gcdd :: Integer -> Integer -> Integer
gcdd a b
  | a < b = gcdd b a
gcdd a 0 = a
gcdd a b = gcdd b (a `mod` b)

llcm :: Integer -> Integer -> Integer
llcm a b = (a * b) `div` (gcdd a b)

readInput :: IO (Integer, Integer)
readInput = readLn >>= \a -> readLn >>= \b -> pure (a, b)

printOutput :: Integer -> Integer -> IO ()
printOutput g l = print g >> print l

main :: IO ()
main = readInput >>= ((\(a, b) -> printOutput (gcdd a b) (llcm a b)))
