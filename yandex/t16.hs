import           Control.Monad
import           Text.Printf
  {-
calc :: [Int] -> [Int] -> (Int, Int)
calc l1 l2 =
  (fst .
   foldr
     (\x@(_, sq) acc@(_, sqacc) ->
        if sqacc > sq
          then x
          else acc)
     x)
    xs
  where
    combs :: [Int] -> [Int] -> [(Int, Int)]
    combs l1 l2 = do
      x1 <- l1
      x2 <- l2
      return (x1, x2)
    findTable :: [(Int, Int)] -> [((Int, Int), Int)]
    findTable lst = do
      (a, b) <- lst
      (c, d) <- lst
      return ((max a b, c + d), max a b * (c + d))
    (x:xs) = findTable $ combs l1 l2-}

calc :: [Int] -> [Int] -> (Int, Int)
calc [a, b] [c, d] =
  (fst .
   foldr
     (\x@(_, sq) acc@(_, sqacc) ->
        if sqacc > sq
          then x
          else acc)
     var1)
    [var2, var3, var4]
  where
    var1 = ((max a c, b + d), max a c * (b + d))
    var2 = ((max b d, a + c), max b d * (a + c))
    var3 = ((max a d, b + c), max a d * (b + c))
    var4 = ((max b c, a + d), max b c * (a + d))

main = do
  [a, b, c, d] <- (map read . words) <$> getLine :: IO [Int]
  let (d1, d2) = calc [a, b] [c, d]
  printf "%d %d\n" d1 d2
