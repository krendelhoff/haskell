data Shape
  = Square
      { side :: Double
      }
  | Rectangle
      { width  :: Double
      , height :: Double
      }
  | Triangle
      { base   :: Double
      , height :: Double
      }
  | Circle
      { radius :: Double
      }
  | CustomShape
      { areaOfShape :: Double
      }
  deriving (Show)

-- тут можно было бы через case
area :: Shape -> Double
area (Square x)      = x * x
area (Rectangle w h) = w * h
area (Triangle b h)  = (1 / 2) * b * h
area (Circle r)      = pi * r * r
area (CustomShape x) = x

instance Ord Shape where
  s1 > s2 = area s1 > area s2
  s1 < s2 = area s1 < area s2
  s1 >= s2 = area s1 >= area s2
  s1 <= s2 = area s1 <= area s2
