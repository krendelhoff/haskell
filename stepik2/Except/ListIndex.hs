module ListIndex where

import           Except

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
list !!! i
  | i < 0 = throwE ErrNegativeIndex
list !!! i = action `catchE` handler
  where
    action = do
      if null list
        then throwE $ ErrIndexTooLarge i
        else if i == 0
               then return (head list)
               else do
                 (tail list) !!! (i - 1)
    handler (ErrIndexTooLarge _) = throwE $ ErrIndexTooLarge i

(!!!!) xs n = runExcept $ xs !!! n
