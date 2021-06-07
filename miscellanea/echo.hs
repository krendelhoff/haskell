echo :: IO ()
echo = getLine >>= \s -> putStrLn s

main = echo
