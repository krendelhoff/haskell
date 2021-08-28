data NestedList a
  = Elem a
  | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a)   = return a
flatten (List lst) = lst >>= flatten
