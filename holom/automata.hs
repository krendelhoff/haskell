import           Control.Monad.State

type FSM s = State s s

fsm :: (ev -> s -> s) -> (ev -> FSM s)
fsm transition = \e -> State $ \s -> (s, transition e s)

type Speaker = (SpeakerState, Level)

data SpeakerState
  = Sleep
  | Work
  deriving (Show)

newtype Level =
  Level Int
  deriving (Show)

data User
  = Button
  | Quieter
  | Louder
  deriving (Show)

quieter :: Level -> Level
quieter (Level n) = Level $ max 0 (n - 1)

louder :: Level -> Level
louder (Level n) = Level $ min 10 (n + 1)

speaker :: User -> FSM Speaker
speaker = fsm $ trans
  where
    trans Button (Sleep, n) = (Work, n)
    trans Button (Work, n)  = (Sleep, n)
    trans Louder (s, n)     = (s, louder n)
    trans Quieter (s, n)    = (s, quieter n)

res = mapM speaker [Button, Louder, Quieter, Quieter, Button]

test = runState res (Sleep, Level 2)

safeSpeaker :: User -> FSM Speaker
safeSpeaker = fsm $ trans
  where
    trans Button (Sleep, _) = (Work, Level 0)
    trans Button (Work, _)  = (Sleep, Level 0)
    trans Quieter (Work, n) = (Work, quieter n)
    trans Louder (Work, n)  = (Work, louder n)
    trans _ (Sleep, n)      = (Sleep, n)

res2 = mapM safeSpeaker [Button, Louder, Quieter, Button, Louder]
