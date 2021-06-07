howMuch :: Integer -> [Integer] -> Integer
howMuch n _ | n <= 1 = 1
howMuch n lst = [ 1 | x <- lst, 
