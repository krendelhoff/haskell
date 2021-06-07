addThree1 :: (Num a) => a -> a -> a -> a
addThree1 x y z = x + y + z

addThree2 :: (Num a) => a -> a -> a -> a
addThree2 = \x -> \y -> \z -> x + y + z

addThree3 :: (Num a) => a -> a -> a -> a
addThree3 = \x y z -> x + y + z
