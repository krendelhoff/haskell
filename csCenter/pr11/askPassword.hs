import           Control.Monad.Except
import           Data.Char            (isNumber, isPunctuation)

newtype PwdError =
  PwdError String
  deriving (Eq, Show)

type PwdErrorMonad = ExceptT PwdError IO

instance Semigroup PwdError where
  (PwdError s1) <> (PwdError s2) = PwdError $ s1 <> s2

instance Monoid PwdError where
  mempty = PwdError ""
  mappend = (<>)

askPassword :: PwdErrorMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  -- msum еще конкатенирует ошибки, поэтому нужен моноид
  liftIO $ putStrLn "Storing in database..."

error1 = "Incorrect input: password is too short!"

error2 = "Incorrect input: password must contain some digits!"

error3 = "Incorrect input: password must contain some punctuations!"

getValidPassword :: PwdErrorMonad String
getValidPassword = do
  s <- liftIO getLine
  when (length s < 8) $ liftIO (putStrLn error1) >> throwError (PwdError error1)
  when (all (not . isNumber) s) $
    liftIO (putStrLn error2) >> throwError (PwdError error2)
  when (all (not . isPunctuation) s) $
    liftIO (putStrLn error3) >> throwError (PwdError error3)
  return s
