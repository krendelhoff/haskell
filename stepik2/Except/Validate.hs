import           Except
import           TryRead
import           TrySum

newtype Validate e a =
  Validate
    { getValidate :: Either [e] a
    }

instance Functor (Validate e) where
  fmap h = (pure h <*>)

instance Applicative (Validate e) where
  pure = Validate . Right
  af <*> ax =
    Validate $
    case getValidate af of
      Left l1 ->
        case getValidate ax of
          Left l2   -> Left $ l1 ++ l2
          otherwise -> Left l1
      Right f ->
        case getValidate ax of
          Left l2 -> Left l2
          Right x -> Right $ f x

collectE :: Except e a -> Validate e a
collectE m =
  Validate $
  case runExcept m of
    Left x  -> Left [x]
    Right x -> Right x

validateSum :: [String] -> Validate SumError Integer
validateSum xs =
  sum <$>
  traverse
    (\(i, s) -> collectE $ withExcept (SumError i) $ tryRead s)
    (zip [1 ..] xs)
