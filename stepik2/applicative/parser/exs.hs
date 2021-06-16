import           Control.Applicative hiding (many)
import           Data.Char

{-newtype Prs a =
  Prs
    { runPrs :: String -> Maybe (a, String)
    }

instance Functor Prs where
  fmap f p =
    Prs $ \s ->
      case runPrs p s of
        Nothing     -> Nothing
        Just (a, s) -> Just (f a, s)

instance Applicative Prs where
  pure x = Prs $ \s -> Just (x, s)
  af <*> ax =
    Prs $ \s -> do
      (f, s') <- runPrs af s
      runPrs (f <$> ax) s'

anyChr :: Prs Char
anyChr = Prs f
  where
    f ""     = Nothing
    f (c:cs) = Just (c, cs)

instance Alternative Prs where
  empty = Prs $ const Nothing
  p <|> q = Prs $ \s -> let ps = runPrs p s in
                                   case ps of
                                      Nothing -> runPrs q s
                                      otherwise -> ps

                                      -- да, в том и прикол,
                                      -- что такое не реализуешь ни через
                                      -- какую монаду, потому что монадические
                                      -- вычисления подразумевают умножение
                                      -- как в аплликативном интерфейсе
                                      -- а тут надо кардинально другое
    -}
newtype Prs a =
  Prs
    { runPrs :: String -> Either String (a, String)
    }

instance Functor Prs where
  fmap f p =
    Prs $ \s ->
      case runPrs p s of
        Left x       -> Left x
        Right (a, s) -> Right (f a, s)

instance Applicative Prs where
  pure x = Prs $ \s -> Right (x, s)
  af <*> ax =
    Prs $ \s -> do
      (f, s') <- runPrs af s
      runPrs (f <$> ax) s'

anyChr :: Prs Char
anyChr = Prs f
  where
    f ""     = Left "unexpected end of input"
    f (c:cs) = Right (c, cs)

satisfy :: (Char -> Bool) -> Prs Char
satisfy pr =
  Prs $ \s -> do
    (ch, s') <- runPrs anyChr s
    if pr ch
      then return (ch, s')
      else Left $ "unexpected " ++ [ch]

{-
satisfyE :: (Char -> Bool) -> PrsE Char
satisftyE pr = PrsE f
  where
    f "" = Left "unexpected end of input"
    f (c:cs)
      | pr c = Right (c, cs)
      | otherwise = Left $ "unexpected " ++ [c]
-}
char :: Char -> Prs Char
char c = satisfy (== c)
