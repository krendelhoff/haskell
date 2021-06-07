import           Control.Monad
import           Data.Char
import           Data.Set      hiding (map, null)

duplicateEncode :: String -> String
duplicateEncode s = reverse $ helper duplicateSet [] s
  where
    createSet set [] = set
    createSet set (x:xs)
      | x `elem` xs = createSet (x `insert` set) xs
      | otherwise = createSet set xs
    duplicateSet = createSet empty (map (toLower) s)
    helper _ acc [] = acc
    helper set acc (x:xs)
      | (toLower x) `member` set = helper set (')' : acc) xs
      | otherwise = helper set ('(' : acc) xs

main = do
  text <- getLine
  when (not $ null text) $ do
    let encoded = duplicateEncode text
    putStrLn encoded
    main
