import qualified Data.List as L
import           Prelude   hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList []          = empty
  fromList ((k, v):xs) = insert k v (fromList xs)

newtype ListMap k v =
  ListMap
    { getListMap :: [(k, v)]
    }
  deriving (Eq, Show, Read)

instance MapLike ListMap where
  empty = ListMap []
  lookup _ (ListMap []) = Nothing
  lookup k (ListMap (x:xs)) =
    if fst x == k
      then Just $ snd x
      else lookup k (ListMap xs)
  insert k v lST = ListMap (insertH k v lST)
    where
      insertH k v (ListMap []) = [(k, v)]
      insertH k v (ListMap (x:xs)) =
        if fst x /= k
          then x : (insertH k v $ ListMap xs)
          else (k, v) : xs
  delete k lST = ListMap (deleteH k lST)
    where
      deleteH k (ListMap []) = []
      deleteH k (ListMap (x:xs)) =
        if fst x /= k
          then x : (deleteH k $ ListMap xs)
          else xs

main = do
  line <- getLine
  print $ delete "biba" ((read line) :: ListMap String String)
