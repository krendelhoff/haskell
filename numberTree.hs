import           Control.Monad.State

data Tree a
  = Leaf a
  | Fork (Tree a) a (Tree a)
  deriving (Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (make tree) 1
  where
    make :: Tree () -> State Integer (Tree Integer)
    make (Leaf _) = do
      n <- get
      put $ n + 1
      return (Leaf n)
    make (Fork t1 _ t2) = do
      n <- get
      let (t3, n1) = runState (make t1) n
      let (t4, n2) = runState (make t2) (n1 + 1)
      put $ n2
      return (Fork t3 n1 t4)

fork :: Tree () -> Tree () -> Tree ()
fork = (`Fork` ())

leaf, unit :: Tree ()
leaf = Leaf ()

unit = fork leaf leaf

testTree :: Tree ()
testTree =
  fork (fork (fork unit leaf) unit) (fork leaf (fork (fork unit unit) leaf))
