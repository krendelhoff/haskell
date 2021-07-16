infixl 6 :+:

infixl 7 :*:

data Expr
  = Val Int
  | Expr :+: Expr
  | Expr :*: Expr
  deriving (Show, Eq)

expand :: Expr -> Expr
expand = foldr1 (:+:) . expandList
  where
    expandList :: Expr -> [Expr]
    expandList (Val i)   = [Val i]
    expandList (l :+: r) = expandList l ++ expandList r
    expandList (l :*: r) = [e1 :*: e2 | e1 <- expandList l, e2 <- expandList r]
