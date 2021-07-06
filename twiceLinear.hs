import           Data.Foldable
import           Data.List

data Tree a
  = Nil
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Foldable Tree where
  foldr f x tree =
    let calc []    = x
        calc level = foldr f (calc (level >>= next)) (level >>= value)
     in calc [tree]
    where
      next Nil          = []
      next (Node l v r) = [l, r]
      value Nil          = []
      value (Node l v r) = [v]

makeTree :: Integer -> Tree Integer
makeTree n = Node (makeTree (2 * n + 1)) n (makeTree (3 * n + 1))

dblLinear :: Int -> Integer
dblLinear n =
  (nub . sort . take (2 ^ (ceiling $ logBase 2 . fromIntegral $ (n + 1))) $
   toList $ makeTree 1) !!
  n
