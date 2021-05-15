import           Data.Void

-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.
-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.
type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (f, g) = (g, f)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (f1, g1) (f2, g2) = (f2 . f1, g1 . g2)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) =
  ((\(a, c) -> (ab a, cd c)), (\(b, d) -> (ba b, dc d)))

isoList :: ISO a b -> ISO [a] [b]
isoList (f, g) = (map f, map g)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (f1, g1) (f2, g2) =
  ( (\x ->
       case x of
         Left y  -> Left $ f1 y
         Right y -> Right $ f2 y)
  , (\x ->
       case x of
         Left y  -> Left $ g1 y
         Right y -> Right $ g2 y))

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (f1, g1) (f2, g2) = ((\f -> f2 . f . g1), (\f -> g2 . f . f1))

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (f, g) =
  ( (\a ->
       case f . return $ a of
         Nothing ->
           let (Just b) = f Nothing
            in b
         (Just b) -> b)
  , (\b ->
       case g . return $ b of
         Nothing ->
           let (Just a) = g Nothing
            in a
         (Just a) -> a))

-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.
-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU =
  ( (\x ->
       case x of
         (Left y)   -> Left $ () : y
         (Right ()) -> Left [])
  , (\y ->
       case y of
         Left [] -> Right ()
         Left x  -> Left $ tail x))

-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither
-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = ((\(f, g) -> (g, f)), (\(f, g) -> (g, f)))
