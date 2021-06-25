import           Data.List

revRange :: (Char, Char) -> [Char]
revRange =
  unfoldr
    (\(x, y) ->
       if x > y
         then Nothing
         else Just (y, (x, pred y)))

tails' :: [a] -> [[a]]
tails' = foldr (\x (y:ys) -> (x : y) : (y : ys)) [[]]

inits' :: [a] -> [[a]]
inits' = foldr (\x (y:ys) -> y : (map (x :) (y : ys))) [[]]

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

reverse'' :: [a] -> [a]
reverse'' = foldl' (flip (:)) []

-- это и есть техника доп. параметра - делаем функцию от инта и этот инт и есть параметр - похожего можно добиться через возврат упорядоченных пар, содержащих инфу, или зипом списка с инфой и т.д.
(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n
  where
    fun x _ 0 = Just x
    fun x g n = g (n - 1)
    ini = const Nothing

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v
  where
    fun = \f x acc v -> acc (f v x)
    ini = id

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

newtype Preorder a =
  PreO (Tree a)
  deriving (Eq, Show)

newtype Levelorder a =
  LevelO (Tree a)
  deriving (Eq, Show)

newtype Postorder a =
  PostO (Tree a)
  deriving (Eq, Show)

instance Foldable Tree where
  foldMap f Nil              = mempty
  foldMap f (Branch t1 a t2) = foldMap f t1 <> f a <> foldMap f t2

instance Foldable Preorder where
  foldMap f (PreO Nil)              = mempty
  foldMap f (PreO (Branch t1 a t2)) = f a <> foldMap f t1 <> foldMap f t2

instance Foldable Postorder where
  foldMap f (PostO Nil)              = mempty
  foldMap f (PostO (Branch t1 a t2)) = foldMap f t1 <> foldMap f t2 <> f a
