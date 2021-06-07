import           Prelude hiding (elem, filter, head, last, map, maximum,
                          product, reverse, sum)

sum :: (Num a) => [a] -> a
sum = foldl (+) 0

elem y =
  foldl
    (\acc x ->
       if x == y
         then True
         else acc)
    False

map f = foldr (\x -> (f x :)) []

-- map f = foldl (\acc x -> f x : acc) [] дал бы reversed list
map' f = foldl (\acc x -> acc ++ [f x]) []

reverse = foldl (flip (:)) []

maximum =
  foldl1
    (\acc x ->
       if x > acc
         then x
         else acc)

product = foldl1 (*)

filter p =
  foldr
    (\x acc ->
       if p x
         then x : acc
         else acc)
    []

head = foldr1 (\x _ -> x)

-- бред полный, ибо foldr1 внутри по факту через head реализован
-- адекватная реализация head - паттерн мэтчинг
last = foldl1 (\_ x -> x)

-- scanl and scanr записывает все промеждуточные вычисления свертки
-- фильный результат у scanl в конце, у scanr - в начале
main :: IO ()
main = print $ reverse [1, 2, 3]
