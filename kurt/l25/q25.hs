import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO       as TIO
import           System.Environment
import           System.IO

main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  chars <- TIO.hGetContents file
  hClose file
  print $ negate $ T.length chars - B.length (E.encodeUtf8 chars)
