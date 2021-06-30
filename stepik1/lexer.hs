import           Data.Char

data Token
  = Number Int
  | Plus
  | Minus
  | LeftBrace
  | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "" = Nothing
asToken s
  | all isDigit s = Just $ Number $ read $ s
  | s == "(" = Just LeftBrace
  | s == ")" = Just RightBrace
  | s == "+" = Just Plus
  | s == "-" = Just Minus
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words
