import           Control.Monad hiding (guard)
import           Prelude       hiding (filter)

filter :: (a -> Bool) -> [a] -> [a]
filter pred list = do
  x <- list
  guard $ pred x
  return x

guard :: Bool -> [()]
guard True  = return ()
guard False = []

endDates :: [Int]
endDates = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [date | end <- ends, date <- [1 .. end]]

dates' :: [Int] -> [Int]
dates' ends = do
  end <- ends
  date <- [1 .. end]
  return date

dates'' :: [Int] -> [Int]
dates'' ends = ends >>= (\end -> [1 .. end] >>= return)
