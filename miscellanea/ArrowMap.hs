import           Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList []          = empty
  fromList ((k, v):xs) = insert k v (fromList xs)

newtype ArrowMap k v =
  ArrowMap
    { getArrowMap :: k -> Maybe v
    }

instance MapLike ArrowMap where
  empty = ArrowMap (\k -> Nothing)
  lookup k (ArrowMap f) =
    case f k of
      Nothing   -> Nothing
      otherwise -> f k
  insert k v (ArrowMap f) =
    ArrowMap
      (\key ->
         if key == k
           then Just v
           else f key)
  delete k (ArrowMap f) =
    ArrowMap
      (\key ->
         if key == k
           then Nothing
           else f key)

main =
  print $
  getArrowMap
    (delete "biba" (insert "biba" "boba" (insert "bibib" "bobob" empty)))
    "biba"
