infixl 6 :+:

infixl 7 :*:

data Expr
  = Val Int
  | Expr :+: Expr
  | Expr :*: Expr
  deriving (Show, Eq)

expand :: Expr -> Expr
expand expr = helper expr (expandRec expr)
  where
    expandRec ((e1 :+: e2) :*: e) =
      expandRec e1 :*: expandRec e :+: expandRec e2 :*: expandRec e
    expandRec (e :*: (e1 :+: e2)) =
      expandRec e :*: expandRec e1 :+: expandRec e :*: expandRec e2
    expandRec (e1 :+: e2) = expandRec e1 :+: expandRec e2
    expandRec (e1 :*: e2) = expandRec e1 :*: expandRec e2
    expandRec e = e
    helper expr1 expr2
      | expr1 == expr2 = expr1
      | otherwise = helper expr2 (expandRec expr2)

main = print $ expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
