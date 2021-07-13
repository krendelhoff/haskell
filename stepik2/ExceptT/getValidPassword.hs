{-# LANGUAGE DeriveAnyClass #-}

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.Char                  (isNumber, isPunctuation)

data PwdError =
  PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

instance Semigroup PwdError where
  (<>) = mappend

instance Monoid PwdError where
  mempty = PwdError ""
  (PwdError s1) `mappend` (PwdError s2) = PwdError $ s1 ++ s2

error1 = "Incorrect input: password is too short!"

error2 = "Incorrect input: password must contain some digits!"

error3 = "Incorrect input: password must contain some punctuation!"

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  when (length s < 8) $ (liftIO $ putStrLn error1) >> throwE (PwdError error1)
  when (all (not . isNumber) s) $
    (liftIO $ putStrLn error2) >> throwE (PwdError error2)
  when (all (not . isPunctuation) s) $
    (liftIO $ putStrLn $ error3) >> throwE (PwdError error3)
  return s
  -- можно было бы еще через catchE попробовать
