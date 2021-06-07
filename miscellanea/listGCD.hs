import           Data.List
import           Prelude   hiding (gcd)

groupIn [] _  = []
groupIn lst n = take n lst : groupIn (drop n lst) n

gcd :: [[Int]] -> [[Int]] -> [[Int]]
gcd [] _ = []
gcd _ [] = []
gcd l1@([x1, x2]:xs) l2@([y1, y2]:ys)
  | x1 == y1 = [x1, min x2 y2] : gcd xs ys
  | x1 > y1 = gcd l1 ys
  | otherwise = gcd xs l2

main = do
  n <- readLn
  rawLines <- sequence $ replicate n getLine
  let workList =
        map ((flip groupIn) 2 . (map (read :: String -> Int)) . words) $
        rawLines
      gcdLst = foldr gcd (head workList) (tail workList)
      makeShowAble = intercalate " " (map (unwords . map show) gcdLst)
  putStrLn makeShowAble
