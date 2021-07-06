import           Control.Applicative

newtype PrsEP a =
  PrsEP
    { runPrsEP :: Int -> String -> (Int, Either String (a, String))
    }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p = snd . runPrsEP p 0

instance Functor PrsEP where
  fmap h (PrsEP p) = PrsEP f
    where
      f pos s =
        let (i, res) = p pos s
         in case res of
              Right (a, s) -> (i, Right (h a, s))
              Left s       -> (i, Left s)

instance Applicative PrsEP where
  pure x = PrsEP $ \i s -> (i, Right (x, s))
  af <*> ax =
    PrsEP $ \i s ->
      let (i', res) = runPrsEP af i s
       in case res of
            (Left x)        -> (i', Left x)
            (Right (f, s')) -> runPrsEP (f <$> ax) i' s'

instance Alternative PrsEP where
  empty = PrsEP $ \i _ -> (i, Left $ "pos " ++ show i ++ ": empty alternative")
  p1 <|> p2 =
    PrsEP $ \i s ->
      case runPrsEP p1 i s of
        (i', Right (a, s')) -> (i', Right (a, s'))
        (i', Left x) ->
          case runPrsEP p2 i s of
            (i'', Left y) ->
              if i' >= i''
                then (i', Left x)
                else (i'', Left y)
            (i'', Right (a, s')) -> (i'', Right (a, s'))

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP p =
  PrsEP $ \i s ->
    let pos = succ i
     in ( pos
        , case s of
            [] -> Left $ showErr pos "end of input"
            (c:cs) ->
              if p c
                then Right (c, cs)
                else Left $ showErr pos [c])
  where
    showErr pos end = "pos " ++ show pos ++ ": unexpected " ++ end
