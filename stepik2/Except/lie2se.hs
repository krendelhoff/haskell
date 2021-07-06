{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Except
import           ListIndex

newtype SimpleError =
  Simple
    { getSimple :: String
    }
  deriving (Eq, Show, Semigroup, Monoid)

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex     = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
