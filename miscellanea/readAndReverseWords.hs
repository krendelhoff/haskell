reverseWords = unwords . map reverse . words

mainn = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

main = do
  str <- return "biba"
  putStrLn str
