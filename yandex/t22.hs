import           Data.List
import           Prelude   hiding (Semigroup, (<>))

data SeqType
  = CONSTANT
  | ASCENDING
  | DESCENDING
  | WEAKLY_ASCENDING
  | WEAKLY_DESCENDING
  | RANDOM

instance Show SeqType where
  show CONSTANT          = "CONSTANT"
  show ASCENDING         = "ASCENDING"
  show DESCENDING        = "DESCENDING"
  show WEAKLY_ASCENDING  = "WEAKLY ASCENDING"
  show WEAKLY_DESCENDING = "WEAKLY DESCENDING"
  show RANDOM            = "RANDOM"

type CheckTuple = (Bool, Bool, Bool, Bool, Bool, Bool)

class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup Bool where
  (<>) = (&&)

instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d
         , Semigroup e
         , Semigroup f
         ) =>
         Semigroup (a, b, c, d, e, f) where
  (a1, a2, a3, a4, a5, a6) <> (b1, b2, b3, b4, b5, b6) =
    (a1 <> b1, a2 <> b2, a3 <> b3, a4 <> b4, a5 <> b5, a6 <> b6)

findType :: [Int] -> SeqType
findType [] = RANDOM
findType [x] = CONSTANT
findType l@(x:xs) = getType $ findTypeH l (True, True, True, True, True, True)
  where
    types =
      [ CONSTANT
      , ASCENDING
      , DESCENDING
      , WEAKLY_ASCENDING
      , WEAKLY_DESCENDING
      , RANDOM
      ]
    findTypeH :: [Int] -> CheckTuple -> CheckTuple
    findTypeH [y, z] tuple   = newTuple y z <> tuple
    findTypeH (y:z:ys) tuple = findTypeH (z : ys) $ (newTuple y z <> tuple)
    newTuple :: Int -> Int -> CheckTuple
    newTuple y z = ((x == z && x == y), y < z, y > z, y <= z, y >= z, True)
    getType :: CheckTuple -> SeqType
    getType (a1, a2, a3, a4, a5, a6) =
      ((\(Just x) -> fst x) $ find snd $ zip types [a1, a2, a3, a4, a5, a6])

readSeq :: IO [Int]
readSeq = do
  n <- read <$> getLine
  if n == (-2 * 10 ^ 9)
    then return []
    else do
      ns <- readSeq
      return (n : ns)

main = readSeq >>= print . findType
