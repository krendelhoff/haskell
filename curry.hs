
main :: IO ()
main = print $ (uncurry (flip (,))) (1, 2)
