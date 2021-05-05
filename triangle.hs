import           Data.List ((\\))

rgb = ['R', 'G', 'B']

triangle :: String -> String
triangle [] = []
triangle [x] = [x]
triangle lst = triangle . reduce $ lst
  where
    reduce [_] = []
    reduce (x:y:xs) =
      if x == y
        then x : (reduce (y : xs))
        else (head $ rgb \\ [x, y]) : (reduce (y : xs))

main = print $ triangle "RRGBRGBB"
