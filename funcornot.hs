import           Data.List

boba :: IO ()
boba = do
  m <- readLn
  lst <- sequence $ replicate m getLine
  let check lst =
        let domain = map ((read :: String -> Int) . head . words) lst
         in nub domain == domain
  putStrLn $
    if check lst
      then "YES"
      else "NO"

main = do
  n <- readLn
  sequence_ $ replicate n boba
