data BoolFunc = BoolFunc (Bool -> Bool)

instance Eq (BoolFunc) where
    (BoolFunc f) == (BoolFunc g) = (f True == g True) && (f False == g False)

f :: Bool -> Bool
f = id

g :: Bool -> Bool
g True = True
g False = False

main :: IO ()
main = print (BoolFunc f == BoolFunc g)

