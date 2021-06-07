import           Data.List

data LogLevel
  = Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

data Person =
  Person
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    }

abbrFirstName :: Person -> Person
abbrFirstName p =
  let fname = firstName p
   in p
        { firstName =
            if length fname == 2
              then fname
              else (head fname) : "."
        }

data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)

treeSum :: Tree Integer -> Integer
treeSum (Leaf a)       = a
treeSum (Node t1 a t2) = a + treeSum t1 + treeSum t2

treeHeight :: Tree a -> Int
treeHeight (Leaf a)       = 1
treeHeight (Node t1 a t2) = 1 + max (treeHeight t1) (treeHeight t2)

sum3 :: (Num a) => [a] -> [a] -> [a] -> [a]
sum3 = zipWith3 (\x y z -> x + y + z)

digits :: Integer -> [Integer]
digits 0 = [0]
digits n = digitsH n
  where
    digitsH 0 = []
    digitsH n = (n `mod` 10) : digitsH (n `div` 10)

containAllDigits :: Integer -> Bool
containAllDigits = (== [1 .. 9]) . sort . nub . digits

containAllDigitsOnes :: Integer -> Bool
containAllDigitsOnes = (== [1 .. 9]) . sort . digits

subList :: Int -> Int -> [a] -> [a]
subList n k = take (k - n) . drop n

movingLists :: Int -> [a] -> [[a]]
movingLists _ []         = []
movingLists n lst@(x:xs) = take n lst : movingLists n xs
