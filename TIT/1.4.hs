fi :: (b -> a) -> (c -> a) -> Either b c -> a
fi = either

gi :: (Either b c -> a) -> (b -> a, c -> a)
gi h = (h . Left, h . Right)

fii :: (c -> (a, b)) -> (c -> a, c -> b)
fii f = (fst . f, snd . f)

gii :: (c -> a, c -> b) -> (c -> (a, b))
gii (h1, h2) = \c -> (h1 c, h2 c)

fiii :: (b -> c -> a) -> (b, c) -> a
fiii = uncurry

giii :: ((b, c) -> a) -> (b -> c -> a)
giii = curry
