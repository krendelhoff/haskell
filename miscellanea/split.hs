solutionHelper :: String -> [String] -> [String]
solutionHelper s sl
        | len == 0      = sl
        | len == 1      = sl ++ [s ++ "_"]
        | otherwise     = solutionHelper (drop 2 s) (sl ++ [[head s] ++ [head (tail s)]])
        where len = (length s)

solution :: String -> [String]
solution s = solutionHelper s []

s = "abcdef"
main :: IO ()
main = print (solution s)
