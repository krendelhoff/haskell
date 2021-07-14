module FSM
  ( FSM(..)
  , fsm
  , module Control.Monad.State
  ) where

import           Control.Monad.State

type FSM s = State s s

type Speaker = (SpeakerState, Level)

data SpeakerState
  = Sleep
  | Work
  deriving (Show)

newtype Level =
  Level Int
  deriving (Show)

data Event
  = Button
  | Quieter
  | Louder
  deriving (Show)

fsm :: (ev -> s -> s) -> (ev -> FSM s)
fsm transition = \e -> state $ \s -> (s, transition e s)

quieter :: Level -> Level
quieter (Level n) = Level $ max 0 (pred n)

louder :: Level -> Level
louder (Level n) = Level $ min 10 (succ n)

speaker :: Event -> FSM Speaker
speaker = fsm $ trans
  where
    trans Button (Sleep, _) = (Work, Level 0)
    trans Button (Work, _)  = (Sleep, Level 0)
    trans Quieter (Work, n) = (Work, quieter n)
    trans Louder (Work, n)  = (Work, louder n)
    trans _ (Sleep, n)      = (Sleep, n)
