data Nat
  = Zero
  | Suc Nat
  deriving (Show, Eq)

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero m    = m
add (Suc n) m = Suc (add n m)

mul :: Nat -> Nat -> Nat
mul Zero _    = Zero
mul (Suc n) m = add m (mul n m)

fac :: Nat -> Nat
fac Zero      = Suc Zero
fac n@(Suc m) = mul n (fac m)

-- естественные рекурсивные определения
main = print $ fac (Suc (Suc (Suc Zero)))
