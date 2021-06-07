evenOnly :: [a] -> [a]
evenOnly xs =
  snd .
  foldr
    (\x (y, z) ->
       if p y
         then (y + 1, x : z)
         else (y + 1, z))
    (1, []) $
  xs
  where
    p =
      if even $ length xs
        then odd
        else even

main = print $ evenOnly [1 .. 10]
