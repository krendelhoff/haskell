{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment
import           System.IO

capitalizeFile :: FilePath -> IO ()
capitalizeFile path = do
  file <- openFile path ReadMode
  contents <- TIO.hGetContents file
  hClose file
  file <- openFile path WriteMode
  TIO.hPutStr file $ cap contents
  hClose file
  where
    cap :: T.Text -> T.Text
    cap = T.unlines . map T.toTitle . T.lines

main = do
  args <- getArgs
  mapM_ capitalizeFile args
