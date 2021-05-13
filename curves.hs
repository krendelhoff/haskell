import           Data.Ratio
import           Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b =
  [ defIntegral ((fromIntegral l) + (segLen / 2)) 0 f
  , defIntegral ((fromIntegral l) + (segLen / 2)) 0 g
  ]
  where
    segLen = 0.001 :: Double
    funcLst = zipWith (\x y -> (x *) . (** y))
    f x =
      foldr
        (\func acc -> func x + acc)
        0
        (funcLst (map fromIntegral a) (map fromIntegral b))
    defIntegral x acc _
      | x > (fromIntegral r) = acc
    defIntegral x acc f = defIntegral (x + segLen) (acc + segLen * (f $ x)) f
    g = (pi *) . (** 2) . f

--Input/Output.
main :: IO ()
main =
  getContents >>=
  mapM_ (printf "%.1f\n") .
  (\[a, b, [l, r]] -> solve l r a b) . map (map read . words) . lines
