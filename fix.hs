import           Data.Function
import           Prelude       hiding (foldr, repeat)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc = fix ff
  where
    ff =
      (\t lst ->
         case lst of
           []     -> acc
           (y:ys) -> f y $ t ys)

repeat :: a -> [a]
repeat = fix . (:)
