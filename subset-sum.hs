import           Control.Monad
import           Data.List

main = do
  listLen <- (readLn :: IO Integer)
  rawLst <- getLine
  testCases <- readLn
  ss <- sequence $ replicate testCases (readLn :: IO Integer)
  let lst = map (read :: String -> Integer) . words $ rawLst
      power = filterM (\x -> [True, False]) lst
      powerSums = map (\x -> (x, sum x)) power
      result =
        map
          (\x ->
             let boba =
                   sortBy
                     (\(_, _, lx) (_, _, ly) ->
                        if lx > ly
                          then GT
                          else LT) .
                   map (\(x, y) -> (x, y, length x)) . filter ((>= x) . snd) $
                   powerSums
              in if null boba
                   then -1
                   else ((\(_, _, x) -> x) $ head boba))
          ss
  mapM_ print result
