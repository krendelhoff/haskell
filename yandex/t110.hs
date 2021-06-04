import           Control.Monad

type Vector a = (a, a)

det :: Vector Double -> Vector Double -> Double
det (a, c) (b, d) = a * d - c * b

solve :: Vector Double -> Vector Double -> Vector Double -> (Int, [Double])
solve (0, 0) (0, 0) (e, f)
  | e /= 0 || f /= 0 = (0, [])
solve v1@(a, c) v2@(b, d) v@(e, f)
  | mainDet /= 0 = (2, map (/ mainDet) [det v v2, det v1 v])
  | det v1 v /= 0 = (0, [])
  | det v2 v /= 0 = (0, [])
  | otherwise =
    if (a, b, e) == (0, 0, 0)
      then infiniteCase c d f
      else infiniteCase a b e
  where
    mainDet = det v1 v2
    infiniteCase :: Double -> Double -> Double -> (Int, [Double])
    infiniteCase 0 0 0 = (5, [])
    infiniteCase 0 b e = (4, [e / b])
    infiniteCase a 0 e = (3, [e / a])
    infiniteCase a b e = (1, [(-a) / b, e / b])

isInt :: Double -> Bool
isInt x = x == (fromIntegral $ round x)

format :: [Double] -> String
format [] = []
format [x] =
  if isInt x
    then show (round x)
    else show x
format (x:xs) =
  mconcat
    [ if isInt x
        then show (round x)
        else show x
    , " "
    , format xs
    ]

main = do
  [a, b, c, d, e, f] <- map read <$> replicateM 6 getLine :: IO [Double]
  let v1 = (a, c)
      v2 = (b, d)
      v = (e, f)
      (res, xs) = solve v1 v2 v
  putStr (show res) >> putStr " " >> (putStrLn . format) xs
