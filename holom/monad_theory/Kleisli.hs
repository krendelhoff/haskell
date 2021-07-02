module Kleisli where

import           Prelude hiding (id, pred, sequence, (*>), (>>))

data Nat
  = Zero
  | Succ Nat
  deriving (Show, Eq, Ord)

class Category cat where
  id :: cat a a
  (>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
  idK :: a -> m a
  (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)

instance Category (->) where
  id = (\x -> x)
  f >> g = g . f

instance Kleisli Maybe where
  idK = Just
  f *> g = f >> maybe Nothing g

pred :: Nat -> Maybe Nat
pred Zero     = Nothing
pred (Succ n) = Just n

pred2 = pred *> pred

pred3 = pred *> pred *> pred

beside = pred +> (\a -> (a, Succ (Succ a)))

next :: Char -> String
next 'a' = "ab"
next 'b' = "a"

instance Kleisli [] where
  idK = \a -> [a]
  f *> g = f >> map g >> concat

generate :: Int -> (a -> [a]) -> (a -> [a])
generate n f = iterate (*> f) idK !! n

gen n = generate n next 'a'

infixr 0 +$, *$

-- a - монадическое значение, поэтому const a - стрелка клейсли
(*$) :: Kleisli m => (a -> m b) -> m a -> m b
f *$ a = (const a *> f) ()

(+$) :: Kleisli m => (a -> b) -> m a -> m b
f +$ a = (const a +> f) ()

gen3 = next *$ next *$ next *$ idK 'a'

boba = next *$ tail $ next *$ reverse $ next *$ idK 'a'

($$) :: Kleisli m => m (a -> b) -> m a -> m b
mf $$ ma = (+$ ma) *$ mf

lift2 :: Kleisli m => (a -> b -> c) -> m a -> m b -> m c
lift2 f a b = (f +$ a) $$ b

-- liftN f x1 x2 .. xN = lift(N-1) x1 .. x(N-1) $$ xN
sequence :: Kleisli m => [m a] -> m [a]
sequence = foldr (lift2 (:)) (idK [])

bibka = sequence [Just 3, Nothing, Just 5]

mapK :: Kleisli m => (a -> m b) -> [a] -> m [b]
mapK f = sequence . map f

newtype State s a =
  State
    { runState :: (s -> (a, s))
    }

instance Kleisli (State s) where
  idK a = State (\s -> (a, s))
  f *> g =
    (\a ->
       State
         (\s ->
            let (b, s1) = runState (f a) s
             in runState (g b) s1))

instance Functor (State s) where
  fmap f ma =
    State
      (\s ->
         let (a, s1) = runState ma s
          in (f a, s1))

-- последовательность передачи состояний может быть выбрана двумя способами - но непонятно как лучше, нужно уметь практически много и хорошо применять, и тогда ясно будет
instance Applicative (State s) where
  pure = idK
  mf <*> ma =
    State
      (\s ->
         let (a, s1) = runState ma s
             (f, s2) = runState mf s1
          in (f a, s2))

instance Monad (State s) where
  return = idK
  ma >>= f -- (const ma *> f) ()
   =
    State
      (\s ->
         let (a, s1) = runState ma s
          in runState (f a) s1)

newtype Reader env b =
  Reader
    { runReader :: env -> b
    }

instance Kleisli (Reader env) where
  idK a = Reader (\env -> a)
  f *> g =
    (\a ->
       Reader
         (\env ->
            let b = runReader (f a) env
             in runReader (g b) env))

instance Functor (Reader env) where
  fmap f ma =
    Reader
      (\env ->
         let a = runReader ma env
          in f a)

instance Applicative (Reader env) where
  pure = idK
  mf <*> ma =
    Reader $ \env ->
      let f = runReader mf env
          a = runReader ma env
       in f a

instance Monad (Reader env) where
  return = idK
  ma >>= f =
    Reader
      (\env ->
         let b = runReader ma env
          in runReader (f b) env)

newtype Writer msg b =
  Writer
    { runWriter :: (b, msg)
    }

instance Monoid msg => Kleisli (Writer msg) where
  idK a = Writer (a, mempty)
  f *> g =
    \a ->
      let (b, msg1) = runWriter (f a)
          (c, msg2) = runWriter (g b)
       in Writer (c, msg1 `mappend` msg2)

instance Monoid msg => Functor (Writer msg) where
  fmap f ma =
    let (a, msg) = runWriter ma
     in Writer (f a, msg)

-- тут тоже двумя способами можно
instance Monoid msg => Applicative (Writer msg) where
  pure = idK
  mf <*> ma =
    let (f, msg1) = runWriter mf
        (a, msg2) = runWriter ma
     in Writer (f a, msg1 `mappend` msg2)

instance Monoid msg => Monad (Writer msg) where
  return = idK
  ma >>= f =
    let (a, msg1) = runWriter ma
        (b, msg2) = runWriter (f a)
     in Writer (b, msg1 `mappend` msg2)

data BTree a
  = BList a
  | BNode a (BTree a) (BTree a)

treeConcat :: BTree (BTree a) -> BTree a
treeConcat (BList t) = t
treeConcat (BNode (BList a) t1 t2) = BNode a (treeConcat t1) (treeConcat t2)
treeConcat (BNode (BNode a tt1 tt2) t1 t2) =
  let concated1 = treeConcat t1
      concated2 = treeConcat t2
   in BNode
        a
        (treeInsert tt1 concated1 concated2)
        (treeInsert tt2 concated1 concated2)
  where
    treeInsert :: BTree a -> BTree a -> BTree a -> BTree a
    treeInsert (BNode a t1 t2) tt1 tt2 =
      BNode a (treeInsert t1 tt1 tt2) (treeInsert t2 tt1 tt2)
    treeInsert (BList a) t1 t2 = BNode a t1 t2

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree (BList a) f       = BList $ f a
mapTree (BNode a t1 t2) f = BNode (f a) (mapTree t1 f) (mapTree t2 f)

instance Kleisli BTree where
  idK = BList
  f *> g = (\a -> treeConcat $ (flip mapTree) g $ f a)

instance Functor BTree where
  fmap = flip mapTree

instance Applicative BTree where
  pure = idK
  mf <*> ma = treeConcat $ mapTree mf $ mapTree ma

instance Monad BTree where
  return = idK
  ma >>= mf = treeConcat $ mapTree ma mf
{-instance Kleisli m => Monad m where
  return = idK
  ma >>= mf = (\_ -> ma) *> mf $ ()

instance Monad m => Kleisli m where
  idK = return
  f *> g = (\a -> f a >>= g)-}
