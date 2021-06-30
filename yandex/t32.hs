import           Data.List
import qualified Data.Set  as S

showlist :: [Int] -> String
showlist []     = ""
showlist (x:xs) = mconcat [show x, " ", showlist xs]

main = do
  l1 <- (map read . words) <$> getLine :: IO [Int]
  l2 <- (map read . words) <$> getLine :: IO [Int]
  putStrLn $ showlist . sort . intersect l1 $ l2
