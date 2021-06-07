import           Data.Char
import           Data.List
import           Data.List.Split

punctuation =
  '\n' :
  ' ' :
  (map
     chr
     (filter
        (\x ->
           let ch = chr x
            in isPunctuation ch && ch /= '\'')
        [0 .. 127]))

wordFrequency :: String -> [(String, Int)]
wordFrequency = sortTupleList . makeValidTupleList
  where
    transform s = map toLower s
    makeList = group . sort . splitOneOf punctuation . transform
    makeTupleList s = map (\(x:xs) -> (x, (length xs) + 1)) (makeList s)
    checkValid s = (not $ null s) && (any isAlpha s)
    makeValidTupleList = (filter (\(x, _) -> checkValid x)) . makeTupleList
    sortTupleList =
      sortBy
        (\(_, n1) (_, n2) ->
           if n1 > n2
             then LT
             else GT)

top3 :: [Char] -> [[Char]]
top3 s
  | len < 3 = take len (map (\(x, _) -> x) list)
  | otherwise = take 3 (map (\(x, _) -> x) list)
  where
    list = wordFrequency s
    len = length list

-- в большинстве случаев explicit recursion вообще не нужна - используй классические функциональные fold, map и filter
main :: IO ()
main = do
  s <- getLine
  print $ top3 s
