import           Data.List

rle :: String -> String
rle [] = []
rle (s:ss) = pack s (length b) ++ rle bs
  where
    (b, bs) = span (== s) ss
    pack s num =
      (if num > 0
         then (s : show (num + 1))
         else [s])
