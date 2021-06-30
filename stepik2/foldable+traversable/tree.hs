data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

newtype Preorder a =
  PreO (Tree a)
  deriving (Eq, Show)

newtype Postorder a =
  PostO (Tree a)
  deriving (Eq, Show)

newtype Levelorder a =
  LevelO (Tree a)
  deriving (Eq, Show)

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch t1 x t2) =
    (\i -> foldr f i t1) . f x . (\i -> foldr f i t2) $ ini

instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch t1 x t2)) =
    f x . (\i -> foldr f i (PreO t1)) . (\i -> foldr f i (PreO t2)) $ ini

instance Foldable Postorder where
  foldr f ini (PostO Nil) = ini
  foldr f ini (PostO (Branch t1 x t2)) =
    (\i -> foldr f i (PostO t1)) . (\i -> foldr f i (PostO t2)) . f x $ ini

type Queue a = ([a], [a])

empty = ([], []) :: Queue a

enqueue :: a -> Queue a -> Queue a
enqueue a (i, o) = (reverse (a : o), a : o)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue ([], [])    = (Nothing, empty)
dequeue ((i:is), o) = (Just i, (is, reverse is))

instance Foldable Levelorder where
  foldr f ini (LevelO Nil) = ini
  foldr f ini (LevelO t) = foldrH f ini (enqueue t empty)
    where
      foldrH f ini q =
        let (node, q') = dequeue q
         in case node of
              Nothing -> ini
              Just Nil -> foldrH f ini q'
              Just (Branch t1 x t2) ->
                f x $ foldrH f ini (enqueue t2 (enqueue t1 q'))
