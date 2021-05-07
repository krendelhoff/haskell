import           Data.Char

data Token
  = Number Int
  | Plus
  | Minus
  | LeftBrace
  | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken ")" = Just RightBrace
asToken "(" = Just LeftBrace
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken s
  | all isDigit s = Just $ Number $ ((read s) :: Int)
asToken _ = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = do
  result <- sequence $ map asToken . words $ input
  return result

main = getLine >>= \x -> print $ tokenize x
-- sequence как раз и реализован через рекурсию, о которой ты думаешь
