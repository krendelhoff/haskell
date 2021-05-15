import           Data.List

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM f [] (words st) -- учитывает контекст
  return result
  where
    readMaybe :: (Read a) => String -> Maybe a
    readMaybe st =
      case reads st of
        [(x, "")] -> Just x
        _         -> Nothing
    f :: [Double] -> String -> Maybe [Double]
    f (x:y:ys) "*"    = return (x * y) : ys
    f (x:y:ys) "+"    = return (x + y) : ys
    f (x:y:ys) "-"    = return (y - x) : ys
    f (x:y:ys) "^"    = return (y ** x) : ys
    f (x:xs) "ln"     = return log x : xs
    f xs "sum"        = return [sum xs]
    f xs numberString = liftM (: xs) $ readMaybe numberString
