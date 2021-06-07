import           Prelude hiding (lines, split)

main' = do
  vals <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn vals

myReplicateM n action = mapM (\_ -> action) [1 .. n]

myReplicateM' n action = sequence $ replicate n action

main = do
  input <- getContents
  let lineS = lines input
  mapM (putStrLn . reverse) lineS

lines s =
  let (l, s') = break (== '\n') s
   in l :
      case s' of
        []      -> []
        (_:s'') -> lines s''

split s set =
  let (l, s') = break (`elem` set) s
   in case l of
        [] -> rec s'
        l  -> l : rec s'
  where
    rec s' =
      case s' of
        []      -> []
        (_:s'') -> split s'' set
