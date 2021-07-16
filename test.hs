import           Data.IORef

main = do
  x <- newIORef 5
  writeIORef x 6
  readIORef x
  bib
