module Parser where

import           Control.Monad
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | String String
  | Bool Bool
  | Character Char

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input =
  case parse parseNumber "lisp" input of
    Left err -> "No match: " ++ show err
    Right val ->
      case val of
        Number val -> show val
        _          -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

escapes :: Parser Char
escapes =
  char '\\' *>
  (char '\"' <|> (char 'n' *> pure '\n') <|> (char 'r' *> pure '\r') <|>
   (char 't' *> pure '\t') <|>
   char '\\')

parseString :: Parser LispVal
parseString =
  fmap String (char '"' *> many (escapes <|> noneOf "\"") <* char '"')

parseAtom :: Parser LispVal
parseAtom =
  fmap
    (\atom ->
       case atom of
         "#t" -> Bool True
         "#f" -> Bool False
         _    -> Atom atom)
    ((:) <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol))

parseNumber :: Parser LispVal
parseNumber =
  fmap
    (Number . fst . head)
    ((fmap reads $ many1 digit) <|>
     (fmap readHex $ (try $ char '#' *> char 'x' *> many1 digit)) <|>
     (fmap readOct $ (char '#' *> char 'o' *> many1 digit)))

parseCharacter :: Parser LispVal
parseCharacter =
  fmap Character $
  char '#' *> char '\\' *>
  ((string "space" *> pure ' ') <|> (string "newline" *> pure '\n') <|> anyChar)

parseFloat :: Parser LispVal
parseFloat =
  fmap
    (Float . fst . head . readFloat)
    ((++) <$> ((++) <$> many1 digit <*> string ".") <*> many1 digit)

parseExpr :: Parser LispVal
parseExpr = parseNumber
