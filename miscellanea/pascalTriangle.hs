pascalTriangle :: Int -> [[Int]]
pascalTriangle n = [1 .. n] >>= (\x -> return $ makeRow (x - 1))
  where
    makeRow n =
      [ (product [1 .. n]) `div` (product [1 .. r] * product [1 .. (n - r)])
      | r <- [0 .. n]
      ]

{-putPascalLn :: [Int] -> IO ()
putPascalLn = putStrLn . foldr (\x acc -> show x ++ acc) []-}
-- без выпендрежа с foldr проще реализовать
-- но в дальшнейшем НИКОГДА explicit recursion не используем, только monad, fuctor, map, filter, concatMap, sequence, mapM, foldr и foldl
--
putPascalLn :: [Int] -> IO ()
putPascalLn = putStrLn . makeStr
  where
    makeStr []     = []
    makeStr [x]    = show x
    makeStr (x:xs) = show x ++ " " ++ makeStr xs

main = do
  n <- readLn
  mapM_ putPascalLn $ pascalTriangle n
