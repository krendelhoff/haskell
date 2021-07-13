import           Control.Monad.Trans
import           ExceptT

data Tile
  = Floor
  | Chasm
  | Snake
  deriving (Show)

data DeathReason
  = Fallen
  | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)

type GameMap = Point -> Tile

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gm n p = runExceptT $ go gm n p
  where
    go :: GameMap -> Int -> Point -> ExceptT DeathReason [] Point
    go gm 0 p = do
      case gm p of
        Floor -> return p
        Chasm -> except . Left $ Fallen
        Snake -> except . Left $ Poisoned
    go gm n p@(x, y) = do
      case gm p of
        Floor -> do
          p <- lift [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
          go gm (n - 1) p
        Chasm -> except . Left $ Fallen
        Snake -> except . Left $ Poisoned

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise = Chasm

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie reason gm n p =
  length $
  filter
    (\x ->
       case x of
         Left x    -> x == reason
         otherwise -> False)
    (moves gm n p)
