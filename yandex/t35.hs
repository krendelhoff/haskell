import qualified Data.Set as S

main = do
  s1 <- (S.fromList . map read . words) <$> getLine
  s2 <-
    (S.fromList . map ((read :: String -> Int) . (return :: Char -> [Char]))) <$>
    getLine
  print $ S.size . S.difference s2 $ s1
