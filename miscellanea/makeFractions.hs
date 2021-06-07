newtype Fraction = Fraction (Int, Int) deriving (Show)

makeFraction :: (Int, Int) -> Either String Fraction
makeFraction (_, 0) = Left "Denominator Cannot be 0"
makeFraction (n, d) | d < 0     = Left "Denominator cannot be negative"
                    | otherwise = Right (Fraction (n, d))

main :: IO ()
main = do
    print ((\(Right (Fraction (x, y))) -> (x, y)) $ makeFraction (3, 5))
    print (makeFraction (3, 0))
    print (makeFraction (9, -3439847329))
    print ((g . f))

-- суть в том, что так мы можем обрабатывать ошибки
-- но результат такого значения мы можем использовать, очевидно, только с полиморфными функциями, например show, ведь мы определили как показывать и Left, и Right
-- вот лямбда-функциями можно легко деконструировать
