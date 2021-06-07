digitalRoot :: (Integral a) => a -> a
digitalRoot 0 = 0
digitalRoot a =
  if root >= 10
    then digitalRoot root
    else root
  where
    makeList 0 = []
    makeList a = (a `mod` 10) : makeList (a `div` 10)
    root = sum $ makeList a

main :: IO ()
main = do
  n <- readLn
  let dR = digitalRoot n
  print dR
