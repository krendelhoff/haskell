data Bit
  = O
  | I
  deriving (Show, Ord, Eq)

newtype StreamCipher =
  StreamCipher String
  deriving (Show, Eq, Ord)

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

type Bits = [Bit]

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    intToBits' 0 = [O]
    intToBits' 1 = [I]
    intToBits' n =
      let rem = n `mod` 2
          next = n `div` 2
       in if (rem == 0)
            then O : intToBits' next
            else I : intToBits' next
    maxBits = length $ intToBits' (maxBound :: Int)
    reversedBits = reverse $ intToBits' n
    missingBits = maxBits - (length reversedBits)
    leadingFalses = replicate missingBits O

bitsToInt :: Bits -> Int
bitsToInt bits = sum $ map (\(_, y) -> 2 ^ y) trueLocations
  where
    size = length bits
    indices = [size - 1,size - 2 .. 0]
    trueLocations = filter (\(x, _) -> x == I) $ zip bits indices

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

instance Cipher StreamCipher where
  encode (StreamCipher stream) text = applyOTP stream text
  decode = encode

myStreamCipher :: StreamCipher
myStreamCipher = StreamCipher $ map toEnum $ iterate examplePRNG 150

xor :: Bit -> Bit -> Bit
xor O O = O
xor O I = I
xor I O = I
xor I I = O

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where
    applyOTP' pad plaintext =
      map (\(x, y) -> zipWith xor x y) (zip padBits plaintextBits)
      where
        padBits = map charToBits pad
        plaintextBits = map charToBits plaintext
    bitList = applyOTP' pad plaintext
