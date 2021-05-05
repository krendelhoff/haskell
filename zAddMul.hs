import           Data.List

data Bit
  = Zero
  | One

data Sign
  = Minus
  | Plus

data Z =
  Z Sign [Bit]

convertZtoInt :: Z -> Int
convertZtoInt (Z sign lst) =
  (convertSign sign) *
  ((fst .
    foldl
      (\(res, count) x -> (res + (convertBit x) * 2 ^ count, count + 1))
      (0, 0))
     lst)
  where
    convertBit :: Bit -> Int
    convertBit Zero = 0
    convertBit One  = 1
    convertSign :: Sign -> Int
    convertSign Minus = -1
    convertSign Plus  = 1

convertIntToZ :: Int -> Z
convertIntToZ x =
  Z (convertToSign x)
    (unfoldr
       (\x ->
          if x > 0
            then Just (convertToBit (x `mod` 2), x `div` 2)
            else Nothing)
       (abs x))
  where
    convertToSign :: Int -> Sign
    convertToSign x =
      if x >= 0
        then Plus
        else Minus
    convertToBit x =
      if x == 0
        then Zero
        else One

add :: Z -> Z -> Z
add z1 z2 = convertIntToZ $ convertZtoInt z1 + convertZtoInt z2

mul :: Z -> Z -> Z
mul z1 z2 = convertIntToZ $ convertZtoInt z1 * convertZtoInt z2

main = print $ add (Z Minus [Zero, One]) (Z Plus [Zero, One])
