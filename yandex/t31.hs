import qualified Data.Set as S

main =
  (S.size . S.fromList . map (read :: String -> Int) . words) <$> getLine >>=
  print
