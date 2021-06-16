newtype Endo a =
  Endo
    { appEndo :: a -> a
    }

instance Semigroup (Endo a) where
  e1 <> e2 = Endo $ appEndo e1 . appEndo e2

instance Monoid (Endo a) where
  mempty = Endo {appEndo = id}
  mappend = (<>)

fn = mconcat $ map Endo [(+ 5), (* 3), (^ 2)]

commConcat :: [String] -> String
commConcat = foldr1 (\x acc -> x ++ (',' : acc))

or' :: [Bool] -> Bool
or' = foldr (||) False

length' :: [a] -> Int
length' = foldr (const succ) 0

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max

head' :: [a] -> a
head' = foldr1 const

last' :: [a] -> a
last' = foldr1 (flip const)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p =
  foldr
    (\x acc ->
       if p x
         then x : acc
         else acc)
    []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

take' :: Int -> [a] -> [a]
take' n xs = foldr step ini xs n
  where
    step :: a -> (Int -> [a]) -> Int -> [a]
    step _ _ 0 = []
    step x g n = x : g (n - 1)
    ini :: Int -> [a]
    ini = const []
