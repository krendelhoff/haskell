import           Control.Applicative
import           Control.Monad.Writer
import qualified Data.Map             as M (Map, fromList, lookup)
import           Data.Monoid
import           Data.Tree

data Exp
  = Var String
  | Lit Int
  | Neg Exp
  | Add Exp Exp
  | Mul Exp Exp
  deriving (Show, Eq)

data Reader env a =
  Reader
    { runReader :: env -> a
    }

instance Functor (Reader env) where
  fmap f ma = ma >>= (\a -> return $ f a)

instance Applicative (Reader env) where
  pure = return
  mf <*> ma = mf >>= (\f -> ma >>= (\a -> return $ f a))

instance Monad (Reader env) where
  return a = Reader $ const a
  ma >>= mf =
    Reader $ \env ->
      let b = runReader ma env
       in runReader (mf b) env

type Env = M.Map String Int

instance Num Exp where
  negate = Neg
  (+) = Add
  (*) = Mul
  fromInteger = Lit . fromInteger
  abs = undefined
  signum = undefined

var :: String -> Exp
var = Var

n :: Int -> Exp
n = var . show

eval :: Exp -> Reader Env Int
eval (Lit n)    = pure n
eval (Neg n)    = liftA negate $ eval n
eval (Add a b)  = liftA2 (+) (eval a) (eval b)
eval (Mul a b)  = liftA2 (*) (eval a) (eval b)
eval (Var name) = Reader $ \env -> value env name

value :: Env -> String -> Int
value env name =
  case M.lookup name env of
    Nothing  -> errorMsg
    (Just x) -> x
  where
    errorMsg = error $ "value is undefined for " ++ name

runExp :: Exp -> [(String, Int)] -> Int
runExp a env = runReader (eval a) $ M.fromList env

countBiFuns :: Exp -> Int
countBiFuns = getSum . execWriter . countBiFuns'

countBiFuns' :: Exp -> Writer (Sum Int) ()
countBiFuns' x =
  case x of
    Add a b -> tell (Sum 1) >> countBiFuns' a >> countBiFuns' b
    Mul a b -> tell (Sum 1) >> countBiFuns' a >> countBiFuns' b
    Neg a   -> countBiFuns' a
    _       -> pure ()

noNeg :: Exp -> Bool
noNeg = not . getAny . execWriter . anyNeg

anyNeg :: Exp -> Writer Any ()
anyNeg x =
  case x of
    Neg _   -> tell (any True)
    Add a b -> anyNeg a >> anyNeg b
    Mul a b -> anyNeg a >> anyNeg b
    _       -> pure ()

type Diap a = (a, a)

inDiap :: Ord a => Diap a -> Tree a -> [a]
inDiap d = execWriter . inDiap' d

-- умно пиздец - через mapM_ мы просто делаем рекурсивно >> ... >>
inDiap' :: Ord a => Diap a -> Tree a -> Writer [a] ()
inDiap' d (Node v xs) = pick d v >> mapM_ (inDiap' d) xs
  where
    pick (a, b) v
      | (a <= v) && (v <= b) = tell [v]
      | otherwise = pure ()
{-
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())-}
-- через Writer можно заебись реализовать обходы деревьев и превращение их в список и много чего другого!!!!!!!!!!!!!!!!!!!!
