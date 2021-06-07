compose :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Int
compose g f x = g (f x)

highOrderFunc :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
highOrderFunc g f = compose g f

main :: IO ()
main = print ((compose (\x -> 2*x) (\x -> 2 + x)) 5)
