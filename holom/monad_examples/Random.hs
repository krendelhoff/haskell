import           Data.Function
import           Types

type Random a = State Double a

data Coin
  = Heads
  | Tails
  deriving (Show)

nextRandom :: Double -> Double
nextRandom = snd . properFraction . (105.947 *)

next :: Random Double
next = State $ \s -> (s, nextRandom s)

addRandom :: Double -> Random Double
addRandom x = fmap (+ x) next

-- складывает два случайных числа из промежутка [a - 1, a + 1] и [-2 + b, b + 2]
addRandom2 :: Double -> Double -> Random Double
addRandom2 a b = liftA2 (add a b) next next
  where
    add a b = \x y -> diap a 1 x + diap b 2 y
    diap c r = \x -> x * 2 * r - r + c

bib =
  let res = fmap sum $ zipWithM addRandom2 [1 .. 3] [11 .. 13]
   in runState res 0.5

dropCoin :: Random Coin
dropCoin = fmap drop' next
  where
    drop' x
      | x < 0.5 = Heads
      | otherwise = Tails

type InitState = Double

playDice :: InitState -> (Integer, Integer)
playDice s =
  foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) . fst $
  runState (replicateM 10 oneRound) s
  where
    addRandomDice :: Random Integer
    addRandomDice = fmap round addRandom2
      where
        addRandom2 = liftA2 add next next
          where
            proper x
              | x < 0.16 = 1
              | x < 0.32 = 2
              | x < 0.48 = 3
              | x < 0.64 = 4
              | x < 0.80 = 5
              | otherwise = 6
            add = (+) `on` proper
    oneRound :: Random (Integer, Integer)
    oneRound = do
      replicateM_ 15 next -- добиваемся полного рандома
      n1 <- addRandomDice
      replicateM_ 15 next
      n2 <- addRandomDice
      return $ (n1, n2)

type Depth = Int

data Tree a
  = Nil
  | Node a [Tree a]
  deriving (Eq, Show)

randomTree :: Depth -> Tree Int
randomTree d
  | d <= 0 = Nil
randomTree d = randomTreeH d 0.2
  where
    get :: Random Double
    get = State $ \st -> (st, st)
    getRandomValues :: Random (Int, Int)
    getRandomValues = do
      replicateM_ 15 next
      value <- (floor . (* 100)) <$> get
      branches <- (round . (* 10)) <$> get
      replicateM_ 15 next
      seed <- get
      return (value, branches)
    randomTreeH 1 seed =
      let ((value, _), _) = runState getRandomValues seed
       in Node value []
    randomTreeH d seed =
      let ((value, branches), seed') = runState getRandomValues seed
       in Node value $ replicate branches (randomTreeH (d - 1) seed')
