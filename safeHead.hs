safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

showHead :: (Show a) => [a] -> String
showHead xs = case safeHead xs of
                Nothing -> "Empty list!"
                Just x  -> show x

main :: IO ()
main = do
    let empty = [] :: [Int]
    putStrLn (showHead [1..5])
    putStrLn (showHead empty)
