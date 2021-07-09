decode c = c $ 0

as x c = c $ x

a x c = c $ x

number = id

one x c = c $ x + 1

two x c = c $ x + 2

three x c = c $ x + 3

seventeen x c = c $ x + 17

twenty x c = c $ x + 20

hundred x c = c $ x * 100

thousand x c = c $ x * 1000
  {-  beautiful
    decode :: (Int -> c) -> c
decode c = c 0

mul :: Int -> Int -> (Int -> r) -> r
mul x y c = c $ x * y

add :: Int -> Int -> (Int -> r) -> r
add x y c = c $ x + y

as :: Int -> (Int -> r) -> r
as x c = c x

a = as

number = id

one = add 1
two = add 2
three = add 3
seventeen = add 17
twenty = add 20
thousand = mul 1000
hundred = mul 100

lambdas are more descriptive
decode = \c -> c 0
as x = \c -> c x
a x = \c -> c x
number = id

one x = \c -> c (x+1)
two x = \c -> c (x+2)
three x = \c -> c (x+3)

seventeen x = \c -> c (x+17)
twenty x = \c -> c (x+20)
hundred x = \c -> c (x*100)
thousand x = \c -> c (x*1000) -}
