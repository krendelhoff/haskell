import           Control.Applicative

(>$<) :: (a -> b) -> [a] -> ZipList b
f >$< lst = ZipList . fmap f $ lst

(>*<) :: ZipList (a -> b) -> [a] -> [b]
af >*< ax = getZipList $ af <*> ZipList ax

data Triple a =
  Tr a a a
  deriving (Eq, Show)

instance Functor Triple where
  fmap h t = pure h <*> t
  -- это мы по сути сфорсили выполнения связующего функтор и аппликатив закона
  -- pure h <*> t == fmap h t

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f g h) <*> (Tr x1 x2 x3) = Tr (f x1) (g x2) (h x3)

instance Foldable Triple where
  foldMap f t =
    let Tr m1 m2 m3 = fmap f t
     in m1 <> m2 <> m3

data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap h Nil              = Nil
  fmap h (Branch t1 a t2) = Branch (fmap h t1) (h a) (fmap h t2)

instance Applicative Tree where
  pure x = Branch Nil x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Branch t1 a1 t2) <*> (Branch t3 a2 t4) =
    Branch (t1 <*> t3) (a1 a2) (t2 <*> t4)
