toStr :: [Double] -> [String]
toStr = map show

main :: IO ()
main = print $ (:) 5 [1]
