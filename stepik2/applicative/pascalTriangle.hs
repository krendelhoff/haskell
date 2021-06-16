pascalTriangle = iterate nextRow [1]
  where
    nextRow xs = (zipWith (+)) <*> tail $ ([0] ++ xs ++ [0])
