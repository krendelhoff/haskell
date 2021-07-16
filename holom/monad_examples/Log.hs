{-# LANGUAGE DeriveAnyClass #-}

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Monoid

data Log
  = F
  | T
  | Not Log
  | Or Log Log
  | And Log Log
  deriving (Eq, Show)

newtype Or' a =
  Or' [a]
  deriving (Show)

runOr' (Or' l) = l

runAnd' (And' l) = l

instance Semigroup (Or' a) where
  (<>) = mappend

instance Monoid (Or' a) where
  mempty = Or' []
  Or' s `mappend` Or' p = Or' $ s ++ p

instance Semigroup (And' a) where
  (<>) = mappend

instance Monoid (And' a) where
  mempty = And' []
  And' s `mappend` And' p = And' $ s ++ p

newtype And' a =
  And' [a]
  deriving (Show)

data Lit
  = True'
  | False'
  | Not' Lit
  deriving (Show, Eq)

type CNF = Or' (And' Lit)

transform :: Log -> CNF
transform log =
  Or'
    (do andLog <- runOr' . execWriter . orTrans . toCNF $ log
        return (execWriter . andTrans $ andLog))
  where
    reduce :: Log -> Log
    reduce (Not (Not a)) = (reduce a)
    reduce (Not (And a b)) = Or (Not (reduce a)) (Not (reduce b))
    reduce (Not (Or a b)) = And (Not (reduce a)) (Not (reduce b))
    reduce (And a (Or b c)) =
      Or (And (reduce a) (reduce b)) (And (reduce a) (reduce c))
    reduce x = x
    toCNF log =
      fst . until (\(p, n) -> p == n) (\(x, y) -> (y, reduce y)) $
      (log, reduce log)
    orTrans :: Log -> Writer (Or' Log) ()
    orTrans (Or a b) = orTrans a >> orTrans b
    orTrans log      = tell $ Or' [log]
    andTrans :: Log -> Writer (And' Lit) ()
    andTrans (And a b) = andTrans a >> andTrans b
    andTrans T         = tell $ And' [True']
    andTrans F         = tell $ And' [False']
    andTrans (Not T)   = tell $ And' [Not' True']
    andTrans (Not F)   = tell $ And' [Not' False']

evalCountLog :: Log -> Int
evalCountLog = getSum . execWriter . evalH
  where
    evalH :: Log -> Writer (Sum Int) ()
    evalH (Not a)   = tell (Sum 1) >> evalH a
    evalH (Or T _)  = tell (Sum 1)
    evalH (Or _ T)  = tell (Sum 1)
    evalH (Or a b)  = tell (Sum 1) >> evalH a >> evalH b
    evalH (And F _) = tell (Sum 1)
    evalH (And _ F) = tell (Sum 1)
    evalH (And a b) = tell (Sum 1) >> evalH a >> evalH b
    evalH _         = return ()

evalCountCNF :: CNF -> Int
evalCountCNF (Or' lst) =
  length (takeWhile (\(x, _) -> x == True') (map eval lst)) - 1 +
  foldr
    (\(x, n) acc ->
       case x of
         True' -> n
         _     -> acc + n)
    0
    (map (process . eval) lst)
  where
    eval :: And' Lit -> (Lit, Int)
    eval =
      foldr
        (\x (acc, n) ->
           case x of
             False'        -> (False', succ n)
             (Not' True')  -> (False', succ (succ n))
             (Not' False') -> (acc, succ (succ n))
             _             -> (acc, succ n))
        (True', 0) .
      runAnd'
    process (x, 0) = (x, 0)
    process (x, y) = (x, pred y)

evalCount :: Log -> (Int, Int)
evalCount a = (evalCountLog a, evalCountCNF . transform $ a)
