import           Control.Monad

doAction :: IO ()
doAction = do
  n <- readLn
  print $ fibonacci n `mod` (10 ^ 8 + 7)

fibonacci :: Int -> Integer
fibonacci = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fibonacci (n - 1) + fibonacci (n - 2)

main = do
  testCases <- readLn
  replicateM_ testCases doAction
