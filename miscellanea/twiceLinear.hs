import           Control.Monad.State
import           Data.List

data Tree a
  = Nil
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

makeTree :: Integer -> Tree Integer
makeTree n = Node (makeTree (2 * n + 1)) n (makeTree (3 * n + 1))

nthFloor :: Integer -> Tree Integer -> Tree Integer
nthFloor n t = evalState (getFloor t) $ 0
  where
    getFloor :: Tree Integer -> State Integer (Tree Integer)
    getFloor (Node t1 t t2) = do
      floor <- get
      if (floor == n)
        then return (Node Nil t Nil)
        else do
          let t1' = evalState (getFloor t1) $ floor + 1
          let t2' = evalState (getFloor t2) $ floor + 1
          return (Node t1' t t2')

extractTree :: Integer -> Tree Integer -> Tree Integer
extractTree m t = nthFloor (round $ logBase 2 (fromIntegral m + 1)) t

toList :: Tree Integer -> [Integer]
toList (Node Nil t Nil) = [t]
toList (Node t1 t t2)   = nub $ foldr insert (toList t1) (t : toList t2)

dblLinear :: Int -> Integer
dblLinear n = toList (extractTree (fromIntegral n) (makeTree 1)) !! n
