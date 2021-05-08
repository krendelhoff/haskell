import           Control.Monad

getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n'
    then return []
    else do
      cs <- getLine'
      return (c : cs)

getLine'' :: IO String
getLine'' =
  getChar >>=
  (\c ->
     if c == '\n'
       then return []
       else getLine'' >>= (\cs -> return (c : cs)))

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr xs

putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar

putStr''' :: String -> IO ()
putStr''' = mapM_ putChar

sequence :: (Monad m) => [m a] -> m [a]
sequence = foldr k (return [])
  where
    k :: (Monad m) => m a -> m [a] -> m [a]
    k m m' = do
      x <- m
      xs <- m'
      return (x : xs)

main = do
  line <- getLine'
  putStrLn line
  putStr' line
  putStr' "\n"
