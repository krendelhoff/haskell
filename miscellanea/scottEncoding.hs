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

fromJust :: SMaybe a -> a
fromJust (SMaybe f) = f (error "it is Nothing!") id

catMaybes :: SList (SMaybe a) -> SList a
catMaybes (SList f) =
  f nihil
    (\h t ->
       if isJust h
         then cons (fromJust h) (catMaybes t)
         else catMaybes t)

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

fromLeft :: SEither a b -> a
fromLeft (SEither f) = f id (error "it is not Left!")

fromRight :: SEither a b -> b
fromRight (SEither f) = f (error "it is not Right!") id

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition (SList f) =
  f (fromPair (nihil, nihil))
    (\h t ->
       let done = partition t
        in if isLeft h
             then fromPair (cons (fromLeft h) $ fst done, snd done)
             else fromPair (fst done, cons (fromRight h) $ snd done))

-- list = (\h t n c -> c h t)
--      = (\n c -> n)
toList :: SList a -> [a]
toList (SList f) = f [] (\h t -> h : toList t)

fromList :: [a] -> SList a
fromList []     = SList (\n c -> n)
fromList (x:xs) = cons x (fromList xs)

cons :: a -> SList a -> SList a
cons h t = SList (\n c -> c h t)

concat :: SList a -> SList a -> SList a
concat (SList f) l = f l (\h t -> cons h (concat t l))

null :: SList a -> Bool
null (SList f) = f True (\_ _ -> False)

length :: SList a -> Int
length = foldr (\_ acc -> acc + 1) 0

map :: (a -> b) -> SList a -> SList b
map f = foldr (\x acc -> cons (f x) acc) nihil

nihil :: SList a
nihil = SList (\n c -> n)

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList f) (SList g) =
  f nihil (\hf tf -> g nihil (\hg tg -> cons (fromPair (hf, hg)) (zip tf tg)))

-- эта функция по своей природе частичная
head :: SList a -> a
head (SList f) = f (error "empty list") const

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f acc l = foldr (\h g x -> g $ f x h) id l acc

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f acc (SList g) = g acc (\h t -> f h (foldr f acc t))

take :: Int -> SList a -> SList a
take n (SList f) = f nihil (\h t -> cons h (take (n - 1) t))
