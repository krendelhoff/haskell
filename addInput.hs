addInput :: IO ()
addInput =
  putStrLn "What's your first number?" >> readLn >>= \first ->
    (putStrLn "What's your second number?" >> readLn >>= \second ->
       putStrLn ("The sum is " ++ (show (first + second)) ++ "!"))

addInputBetter :: IO ()
addInputBetter = do
  putStrLn "What's your first number?"
  first <- readLn
  putStrLn "What's your second number?"
  second <- readLn
  putStrLn ("The sum is " ++ (show (first + second)) ++ ".")

main = addInputBetter
