{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScottEncoding where

import           Prelude hiding (concat, curry, foldl, foldr, fst, head, length,
                          map, null, snd, take, uncurry, zip, (++))

newtype SMaybe a =
  SMaybe
    { runMaybe :: forall b. b -> (a -> b) -> b
    }

newtype SList a =
  SList
    { runList :: forall b. b -> (a -> SList a -> b) -> b
    }

newtype SEither a b =
  SEither
    { runEither :: forall c. (a -> c) -> (b -> c) -> c
    }

newtype SPair a b =
  SPair
    { runPair :: forall c. (a -> b -> c) -> c
    }

tru = const

fls = flip const

-- pair = (\x1 x2 c -> c x1 x2)
toPair :: SPair a b -> (a, b)
toPair p = (fst p, snd p)

fromPair :: (a, b) -> SPair a b
fromPair (a, b) = SPair (\c -> c a b)

fst :: SPair a b -> a
fst (SPair f) = f tru

snd :: SPair a b -> b
snd (SPair f) = f fls

swap :: SPair a b -> SPair b a
swap (SPair f) = SPair $ f . flip

curry :: (SPair a b -> c) -> (a -> b -> c)
curry = (\f x y -> f $ fromPair (x, y))

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry = (\f p -> f (fst p) (snd p))

-- maybe = (\x n j -> j x)
--       = (\n j -> n)
toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe f) = f Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing  = SMaybe const
fromMaybe (Just x) = SMaybe (\n j -> j x)

isJust :: SMaybe a -> Bool
isJust (SMaybe f) = f False (\_ -> True)

isNothing :: SMaybe a -> Bool
isNothing (SMaybe f) = f True (\_ -> False)

catMaybes :: SList (SMaybe a) -> SList a
catMaybes = error "catMaybes"

-- either = (\x l r -> l x)
--        = (\x l r -> r x)
toEither :: SEither a b -> Either a b
toEither (SEither f) = f Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left x)  = SEither (\l r -> l x)
fromEither (Right x) = SEither (\l r -> r x)

isLeft :: SEither a b -> Bool
isLeft (SEither f) = f (\_ -> True) (\_ -> False)

isRight :: SEither a b -> Bool
isRight (SEither f) = f (\_ -> False) (\_ -> True)

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = error "partition"

{-newtype SList a =
  SList
    { runList :: forall b. b -> (a -> SList a -> b) -> b
    }-}
-- list = (\h t n c -> c h t)
--      = (\n c -> n)
toList :: SList a -> [a]
toList l@(SList f) = undefined

head :: SList a -> a
head (SList f) = undefined

fromList :: [a] -> SList a
fromList []     = SList (\n c -> n)
fromList (x:xs) = cons x (fromList xs)

cons :: a -> SList a -> SList a
cons a l = SList (\n c -> c a l)

concat :: SList a -> SList a -> SList a
concat = error "concat"

null :: SList a -> Bool
null (SList f) = f True (\_ _ -> False)

length :: SList a -> Int
length = error "length"

map :: (a -> b) -> SList a -> SList b
map = error "map"

zip :: SList a -> SList b -> SList (SPair a b)
zip = error "zip"

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl = error "foldl"

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr = error "foldr"

take :: Int -> SList a -> SList a
take = error "take"
