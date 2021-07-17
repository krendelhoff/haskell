{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except

data ListIndexError
  = ErrTooLargeIndex Int
  | ErrNegativeIndex
  | OtherErr String
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: (MonadError ListIndexError m) => [a] -> Int -> m a
xs !!! n
  | n < 0 = throwError ErrNegativeIndex
xs !!! n = go xs n
  where
    go [] _     = throwError $ ErrTooLargeIndex n
    go (c:cs) 0 = return c
    go (_:cs) m = go cs (m - 1)

data Excep a
  = Err String
  | Ok a
  deriving (Eq, Show)

instance Monad Excep where
  return = Ok
  m >>= k =
    case m of
      Err str -> Err str
      Ok x    -> k x

instance Functor Excep where
  fmap = liftM

instance Applicative Excep where
  pure = return
  (<*>) = ap

instance MonadError String Excep where
  throwError = Err
  m `catchError` handler =
    case m of
      (Err str) -> handler str
      (Ok x)    -> Ok x

instance Alternative Excep where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Excep where
  mzero = Err "empty"
  (Err _) `mplus` x  = x
  r@(Ok _) `mplus` _ = r

(?/) :: (MonadError String m) => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

instance MonadFail Excep where
  fail _ = mzero

example :: Double -> Double -> Excep String
example x y = action `catchError` return
  where
    action = do
      q <- x ?/ y
      guard (q >= 0)
      if q > 100
        then do
          100 <- return q
          undefined
        else return $ show q

data ParseError =
  ParseError
    { location :: Int
    , reason   :: String
    }

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Integer
parseHex s =
  fmap (flip (foldr g id) (fromIntegral (length s) - 1)) $
  traverse f . zip [0 ..] $ s
  where
    f (_, '0') = return 0
    f (_, '1') = return 1
    f (_, '2') = return 2
    f (_, '3') = return 3
    f (_, '4') = return 4
    f (_, '5') = return 5
    f (_, '6') = return 6
    f (_, '7') = return 7
    f (_, '8') = return 8
    f (_, '9') = return 9
    f (_, x)
      | x == 'a' || x == 'A' = return 10
    f (_, x)
      | x == 'b' || x == 'B' = return 11
    f (_, x)
      | x == 'c' || x == 'C' = return 12
    f (_, x)
      | x == 'd' || x == 'D' = return 13
    f (_, x)
      | x == 'e' || x == 'E' = return 14
    f (_, x)
      | x == 'f' || x == 'F' = return 15
    f (n, x) =
      throwError $
      ParseError
        {location = n, reason = "Unexpected " ++ [x] ++ " at pos " ++ show n}
    g x acc 0 = x
    g x acc n = x * (16 ^ n) + acc (n - 1)
    -- прием дополнительного параметра!
    -- как раз нужно когда индексация какая-нибудь возникает

printError :: ParseError -> ParseMonad String
printError (ParseError {reason = s}) = return s

test s = str
  where
    (Right str) =
      do n <- parseHex s
         return $ show n
     `catchError` printError
