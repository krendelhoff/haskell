newtype State s a =
  State
    { runState :: (s -> (a, s))
    }

instance Functor (State s) where
  fmap f ma =
    State
      (\s ->
         let (a, s1) = runState ma s
          in (f a, s1))

instance Applicative (State s) where
  pure = return
  mf <*> ma =
    State
      (\s ->
         let (a, s1) = runState ma s
             (f, s2) = runState mf s1
          in (f a, s2))

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  ma >>= f =
    State
      (\s ->
         let (a, s1) = runState ma s
          in runState (f a) s1)

type Random a = State Double a

data Dice
  = I
  | II
  | III
  | IV
  | V
  | VI
  deriving (Show, Eq, Ord)

nextRandom :: Double -> Double
nextRandom = snd . properFraction . (105.947 *)

next :: Random Double
next = State $ \s -> (s, nextRandom s)

getDice :: Double -> Dice
getDice x
  | x < 1 / 6 = I
  | x < 2 / 6 = II
  | x < 3 / 6 = III
  | x < 4 / 6 = IV
  | x < 5 / 6 = V
  | otherwise = VI

dropDice :: Random Dice
dropDice = fmap getDice next

oneRound :: Random (Dice, Dice)
oneRound = do
  next
  diceVal1 <- dropDice
  diceVal2 <- dropDice
  return (diceVal1, diceVal2)
