import           Control.Applicative
import           Control.Monad

surround :: a -> a -> [a] -> [a]
surround x y zs = do
  z <- zs
  [x, z, y]

lookups :: (Eq a) => a -> [(a, b)] -> [b]
lookups x ys = do
  (a, b) <- ys
  guard (x == a)
  return b

factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
  x <- filter (\x -> n `mod` x == 0) [1 .. floor $ sqrt $ fromIntegral n]
  return (x, n `div` x)

absDiff :: Num a => [a] -> [a]
absDiff = map (\(a, b) -> abs (a - b)) . (zip <*> tail)

data Cell
  = E
  | X
  | O
  deriving (Eq, Show)

type Row a = [a]

type Board = Row (Row Cell)

iniBoard :: Int -> Board
iniBoard n =
  let row = replicate n E
   in replicate n row

win :: Cell -> Board -> Bool
win E _ = False
win x brd =
  let rowLen = length $ head brd
      searchRow = replicate rowLen x
   in searchRow `elem` brd ||
      searchRow `elem` (transpose brd) ||
      searchRow == map (\n -> brd !! n !! n) [0 .. rowLen - 1]
  where
    transpose = getZipList . sequenceA . map ZipList

nxt :: Cell -> Board -> [Board]
nxt x brd = do
  (n, row) <- zip [0 ..] brd
  (m, cell) <- zip [0 ..] row
  guard (cell == E)
  return (insert x (n, m) brd)
  where
    insert x (n, m) brd =
      take n brd ++
      [ map
          (\(n, y) ->
             if n == m
               then x
               else y)
          (zip [0 ..] $ brd !! n)
      ] ++
      drop (n + 1) brd

doNTurns :: Int -> Board -> [Board]
doNTurns n ini = foldr (>=>) return (list n) ini
  where
    list n =
      let rem = n `mod` 2
       in concat (replicate (n `div` 2) [nxt X, nxt O] ++ replicate rem [nxt X])
