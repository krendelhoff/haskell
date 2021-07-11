import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Char                  (toUpper)

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
  el1 <- lift $ asks head
  el2 <- lift $ asks (map toUpper . head . tail)
  tell el1
  return el2
