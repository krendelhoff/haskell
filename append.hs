append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x:append xs ys


-- многие функции пишутся чисто через pattern matching - fst, snd, head
