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
  | String String
  | Bool Bool
  | Character Char

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right s ->
      case s of
        String s     -> "Found value" ++ ":" ++ s
        Number n     -> "Found Value" ++ ":" ++ show n
        Character ch -> "Found Value" ++ ":" ++ [ch]
        _            -> "Found Value"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escape :: Parser Char
escape = do
  x <- char '\\'
  y <- oneOf "\"nrt\\"
  return $
    case y of
      '\"' -> '\"'
      'n'  -> '\n'
      'r'  -> '\r'
      't'  -> '\t'
      '\\' -> '\\'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escape <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  mode <- string "#o" <|> string "#x" <|> string ""
  x <- many1 digit
  return $
    case mode of
      ""   -> Number . read $ x
      "#o" -> Number . fst . head . readOct $ x
      "#x" -> Number . fst . head . readHex $ x

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  ch <- letter <|> symbol <|> oneOf "() "
  return $ Character ch

parseExpr :: Parser LispVal
parseExpr = parseCharacter <|> parseNumber <|> parseString <|> parseAtom
