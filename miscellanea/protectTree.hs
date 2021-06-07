import           Data.List

decompose :: Integer -> Maybe [Integer]
decompose n = makeSequence (n * n)
  where
    makeSequence k
      | k <= 0 = Nothing
    makeSequence 1 = Just [1]
    makeSequence k =
      foldr
        (\x acc ->
           case acc of
             Nothing ->
               case makeSequence (k - x * x) of
                 Nothing -> Nothing
                 (Just seq) ->
                   if isDublicate seq
                     then Just $ seq ++ [x]
                     else Nothing
             (Just seq) -> Just seq)
        Nothing
        [1 .. lesser]
      where
        isDublicate lst = nub lst == lst
        lesser =
          let rawLesser = floor $ sqrt $ fromIntegral k
           in if rawLesser * rawLesser == k
                then rawLesser - 1
                else rawLesser
