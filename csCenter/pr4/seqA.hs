recSeq :: Integer -> Integer
recSeq n
  | n <= 0 = 1
recSeq 1 = 2
recSeq 2 = 3
recSeq n = recSeqH 1 2 3 (n - 2)
  where
    recSeqH a b c 0 = c
    recSeqH a b c n = recSeqH b c (c + b - 2 * a) (n - 1)
