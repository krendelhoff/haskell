module Stream where

import           Control.Applicative
import           Control.Arrow

data Stream a =
  a :> Stream a
  deriving (Show)

infixr :>

instance (Num a) => Num (Stream a) where
  (+) = zipWithS (+)
  (*) = zipWithS (*)
  (-) = zipWithS (-)
  abs = mapS abs
  signum = mapS signum
  fromInteger n = repeatS $ fromInteger n

data St a b =
  St (a -> (b, St a b))

ap :: St a b -> [a] -> [b]
ap _ [] = []
ap (St f) (x:xs) =
  let (b, newf) = f x
   in b : ap newf xs

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (h :> _) = h

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> stream) = stream

-- {{{ Stream constructors
-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS h = h :> (repeatS h)

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateSH f x
  where
    iterateSH f x = (f x) :> (iterateS f (f x))

mapS :: (a -> b) -> Stream a -> Stream b
mapS f (a :> sa) = f a :> mapS f sa

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = cycleSH xs xs
  where
    cycleSH [] ys     = cycleSH ys ys
    cycleSH (x:xs) ys = x :> (cycleSH xs ys)

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS n = n :> (fromS $ n + 1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> (fromStepS (x + s) s)

-- }}}
-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x $ foldrS f xs

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs) =
  if p x
    then x :> (filterS p xs)
    else (filterS p xs)

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i s = reverse . takeSH i s $ []
  where
    takeSH i _ acc
      | i <= 0 = acc
    takeSH i (h :> s) acc = takeSH (i - 1) s (h : acc)

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s
  | i <= 0 = s
dropS i (h :> s) = dropS (i - 1) s

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (takeS i s, dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
  fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
  pure x = x :> pure x
  (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> 1 :> (zipWithS (+) fibS $ tailS fibS)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = filterS (not . notPrime) $ fromS 2
  where
    notPrime x =
      let final = round $ sqrt $ fromIntegral x
       in any (\y -> x `mod` y == 0) [2 .. final]
