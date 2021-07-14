{-# LANGUAGE MultiWayIf #-}

import           FSM

data Event =
  Light
    { north, south, west, east :: Int
    }

data AmoebaState
  = Warming
  | GoingNorth
  | GoingSouth
  | GoingWest
  | GoingEast
  deriving (Show)

amoeba :: Event -> FSM AmoebaState
amoeba = fsm $ trans
  where
    trans (Light 0 0 0 0) GoingNorth = GoingSouth
    trans (Light 0 0 0 0) GoingSouth = GoingNorth
    trans (Light 0 0 0 0) GoingWest = GoingEast
    trans (Light 0 0 0 0) GoingEast = GoingWest
    trans (Light 0 0 0 0) Warming = GoingWest
    trans p _
      | north p == south p && south p == west p && west p == east p = Warming
    trans (Light n s w e) _ =
      let m = maximum [n, s, w, e]
       in if | m == n -> GoingNorth
             | m == s -> GoingSouth
             | m == w -> GoingWest
             | m == e -> GoingEast
