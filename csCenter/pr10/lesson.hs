import           Control.Monad.State
import           Control.Monad.Writer
import           Data.IORef
import           System.Random

facStep :: State (Integer, Integer) ()
facStep = modify f
  where
    f (f, i) = (f * i, succ i)

fac n = fst $ execState (replicateM n facStep) (1, 0)

facStep' :: Integer -> State Integer ()
facStep' i = modify (* i)

fac' n = execState (forM_ [1 .. n] facStep') 1

fac'' :: Integer -> IO Integer
fac'' n = do
  acc <- newIORef 1
  forM_ [1 .. n] $ \i -> modifyIORef' acc (* i)
  readIORef acc

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState = state . randomR

testWork :: ([Int], [Int])
testWork = evalState doWork (mkStdGen 42)

doWork :: State StdGen ([Int], [Int])
doWork = do
  xs <- replicateM 5 $ randomRState (1, 6)
  ys <- replicateM 5 $ randomRState (1, 6)
  return (xs, ys)

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR ini =
  foldr
    (\x acc -> do
       res <- censor (\log -> '(' : show x ++ "-" ++ log ++ ")") acc
       return (x - res))
    (tell (show ini) >> return ini)

runMinusLoggedR = runWriter . minusLoggedR 0

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL ini =
  foldl
    (\acc x -> do
       res <- censor (\log -> '(' : log ++ "-" ++ show x ++ ")") acc
       return (res - x))
    (tell (show ini) >> return ini)

runMinusLoggedL = runWriter . minusLoggedL 0

fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0, 1)
  where
    fibStep :: State (Integer, Integer) ()
    fibStep = modify (\(a, b) -> (b, a + b))

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  cond <- readIORef ref
  if p cond
    then action >> while ref p action
    -- да, непривычно
    -- action РЕАЛЬНО совершает побочное действие - оно меняет состояние в памяти
    -- а именно - значение к которому обращается ref
    -- то есть да, это ссылка на память и action может его менять
    -- непривычно, т.к. мы не передаем это туда, в общем да..
    -- это именно ссылка и достаточно иметь ссылку, чтобы менять внутри
    else return ()

factorial :: Integer -> IO Integer
factorian n = do
  r <- newIORef 1
  i <- newIORef 1
  while
    i
    (<= n)
    do ival <- readIORef i
       modifyIORef' r (* ival)
       modifyIORef' i (+ 1)
  readIORef r
