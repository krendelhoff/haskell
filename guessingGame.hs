guessingGame :: Int -> IO ()
guessingGame solution = do
  putStrLn "Guess the number!"
  guess <- readLn
  if (guess == solution)
    then putStrLn "Correct!"
    else putStrLn "Wrong!"

guessingGameBetter :: Int -> IO ()
guessingGameBetter solution = do
  putStrLn "Guess the number!"
  guess <- readLn
  let answer =
        if guess == solution
          then "Correct!"
          else "Wrong"
  putStrLn answer

guessingGameNotBad :: Int -> IO ()
guessingGameNotBad solution = do
  putStrLn "Guess the number!"
  guess <- readLn
  putStrLn
    (if guess == solution
       then "Correct!"
       else "Wrong!")

main = guessingGameBetter 10
