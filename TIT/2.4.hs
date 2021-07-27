{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs
