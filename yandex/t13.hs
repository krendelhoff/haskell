import           Control.Monad

clear :: String -> String
clear num =
  if length num == 7
    then '4' : '9' : '5' : num
    else case num of
           ('+':'7':xs) -> xs
           ('8':xs)     -> xs

processNumber :: String -> String
processNumber = clear . filter (\x -> x /= '-' && x /= '(' && x /= ')')

boolToStr :: Bool -> String
boolToStr False = "NO"
boolToStr True  = "YES"

main = do
  l@[num, num1, num2, num3] <- replicateM 4 getLine
  mapM_ (putStrLn . boolToStr . (== (processNumber num))) $
    map processNumber $ tail l
