import           Control.Applicative hiding (many)
import           Data.Char
import           Data.List

newtype Parser a =
  Parser
    { apply :: String -> [(a, String)]
    }

-- это чисто синтаксический шум, который существует только в дезайн-тайме
instance Functor Parser where
  fmap f p = Parser fun
    where
      fun s = [(f a, s') | (a, s') <- apply p s]

instance Applicative Parser where
  pure x = Parser $ \s -> [(x, s)]
  pf <*> pv = Parser fun
    where
      fun s = [(g a, s'') | (g, s') <- apply pf s, (a, s'') <- apply pv s']

instance Alternative Parser where
  empty = Parser $ const [] -- всегда ошибка
  p <|> q =
    Parser $ \s ->
      let ps = apply p s
       in if null ps
            then apply q s
            else ps

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f
  where
    f ""     = []
    f (c:cs) = [(c, cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f
  where
    f "" = []
    f (c:cs)
      | pr c = [(c, cs)]
      | otherwise = []

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

-- парсер можно воспринимать как Maybe, то есть работа аппликатива вполне понятна
multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []

{-many1 :: Parser a -> Parser [a]
many1 p =
  Parser $ \s ->
    let ps = apply p s
     in if null ps
          then []
          else [(a : b, s'') | (a, s') <- ps, (b, s'') <- apply (many p) s']-}
many1 :: Parser a -> Parser [a]
many1 p =
  Parser $ \s -> do
    (res, s') <- apply p s
    (res1, s'') <- apply (many p) s'
    return (res : res1, s'')

skipMany :: Parser a -> Parser ()
skipMany p = (p *> skipMany p) <|> pure ()

skipMany1 :: Parser a -> Parser ()
skipMany1 p = p *> skipMany p

nat :: Parser Int
nat = toInt <$> many1 digit

toInt :: [Int] -> Int
toInt lst = foldr fun (const 0) lst (length lst - 1)
  where
    fun x _ 0   = x
    fun x acc n = x * 10 ^ n + acc (n - 1)

mult :: Parser Int
mult = (*) <$> nat <* char '*' <*> nat
