map ($ 3) [(4 +), (10 *), (^ 2), sqrt]

-- так что это очень важный оператор! обычное применение функции имеет наивысший приоритет и лево ассоциативна, а $ - наименьший и право ассоциативна
-- But what about functions that take several parameters? Well, if we want to use them in function composition, we usually have to partially apply them just so much that each function takes just one parameter.
biba =
  replicate 100 . product . map (* 3) . zipWith max [1, 2, 3, 4, 5] $
  [4, 5, 6, 7, 8]

fn x = ceiling (negate (tan (cos (max 50 x))))

fnPointLess = ceiling . negate . tan . cos . max 50

oddSquareSum = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]
