import           Control.Monad.Writer
import           Data.List

type Symb = String

type Symbs = [String]

infixl 2 :@

-- по дефолту считаем что имена связанных переменных и свободных не пересекаются, при необходимости переименовываем
data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Show)

traverseVars ::
     (Symbs -> Writer Symbs ()) -> (Symbs -> Writer Symbs ()) -> Expr -> Symbs
traverseVars f g = nub . snd . runWriter . (tVarsH f g)
  where
    tVarsH ::
         (Symbs -> Writer Symbs ())
      -> (Symbs -> Writer Symbs ())
      -> Expr
      -> Writer Symbs ()
    tVarsH f g (Var symb) = f [symb]
    tVarsH f g (exp1 :@ exp2) = do
      tVarsH f g exp1
      tVarsH f g exp2
    tVarsH f g (Lam symb exp) = do
      g [symb]
      tVarsH f g exp

vars :: Expr -> Symbs
vars = traverseVars tell tell

boundedVars :: Expr -> Symbs
boundedVars = traverseVars (const (return ())) tell

freeVars :: Expr -> Symbs
freeVars exp = vars exp \\ boundedVars exp

renameVar :: Symb -> Expr -> Expr
renameVar rsym (Var sym) =
  if rsym == sym
    then Var $ '\'' : sym
    else Var sym
renameVar rsym (Lam sym exp) =
  Lam
    (if rsym == sym
       then '\'' : sym
       else sym)
    (renameVar rsym exp)
renameVar rsym (exp1 :@ exp2) = renameVar rsym exp1 :@ renameVar rsym exp2

subst :: Symb -> Expr -> Expr -> Expr
subst v n m = substH v n correctM
  where
    renameVars = intersect (boundedVars m \\ [v]) (freeVars n)
    correctM =
      foldr
        (.)
        id
        (zipWith ($) (replicate (length renameVars) renameVar) renameVars) $
      m
    substH v n (Lam sym exp) =
      if sym == v
        then substH v n exp
        else Lam sym (substH v n exp)
    substH v n (Var sym) =
      if sym == v
        then n
        else Var sym
    substH v n (exp1 :@ exp2) = substH v n exp1 :@ substH v n exp2
-- alphaEq
