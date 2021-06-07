import           Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack Int
push a = State $ \xs -> ((), a : xs)
