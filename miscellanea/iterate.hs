iterate :: (a -> a) -> a -> [a]
iterate f z = [z] ++ iterate f (f z
