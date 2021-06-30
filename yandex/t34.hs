import           Data.List

main = do
  text <- readFile "input.txt"
  print $ length . group . sort . words $ text
