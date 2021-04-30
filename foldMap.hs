foldmap :: (a -> b) -> [a] -> [b]
foldmap f xs = foldr (\x -> (f x :)) [] xs

main :: IO ()
main = print $ foldmap (* 2) [1, 2, 3]
