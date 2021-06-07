times f m = foldr (\x acc -> x . acc) id (replicate m f)

xz = \x -> (x, 0)

fs = \f (x, y) -> (f x y, succ y)

rec = \m f x -> fst $ (times (fs f) m) (xz x)

fac = \n -> rec n (\x y -> x * (y + 1)) 1

sumn = \n -> rec n (\x y -> x + y + 1) 0

sumf = \f n -> rec n (\x y -> x + (f (y + 1))) 0
