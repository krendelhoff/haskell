import           Control.Monad
import           Data.List
import           System.Directory
import           System.IO

main = do
  putStr "Substring: "
  hFlush stdout
  boba <- getLine
  if null boba
    then do
      putStrLn "Canceled"
    else do
      files <- getDirectoryContents "."
      mapM_
        (\name -> do
           putStr "Removing file: "
           putStrLn name
           removeFile name) .
        filter (boba `isInfixOf`) $
        files
