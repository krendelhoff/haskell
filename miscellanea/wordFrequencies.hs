import Data.Char
import Data.List

mmap :: (a -> b) -> [a] -> [b]
mmap _ [] = []
mmap f (x:xs) = f x : mmap f xs

ffilter :: (a -> Bool) -> [a] -> [a]
ffilter _ [] = []
ffilter p (x:xs)
  | p x       = x : ffilter p xs
  | otherwise = ffilter p xs

wordFrequencies :: String -> [(String, Int)]
wordFrequencies s = [ (head x, length x) | x <- list ]
                where
                    clean = ffilter (\x -> isLetter x || x == ' ') . mmap toLower
                    morph = group . sort . words . clean
                    list = morph s
                    
-- тут group зарешала
-- развивай мозг для функциональных решений, постепенно всё придет
--
wordFrequencies2 :: String -> [(String, Int)]
wordFrequencies2 s =
    let clean = ffilter (\x -> isLetter x || x == ' ') . mmap toLower
        morph = group . sort . words . clean
        morphed = morph s
    in map (\xs -> (head xs, length xs)) morphed

main :: IO ()
main = print $ (wordFrequencies2 "It was the best of times, it was the worst of times,")
