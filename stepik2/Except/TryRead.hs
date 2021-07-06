module TryRead where

import           Except

data ReadError
  = EmptyInput
  | NoParse String
  deriving (Show)

tryRead :: Read a => String -> Except ReadError a
tryRead [] = throwE EmptyInput
tryRead s =
  case reads s of
    ((a, ""):_) -> return a
    _           -> throwE $ NoParse s
