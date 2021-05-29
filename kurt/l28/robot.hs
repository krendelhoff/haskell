import qualified Data.Map           as M
import           System.Environment

data RobotPart =
  RobotPart
    { name        :: String
    , description :: String
    , cost        :: Double
    , count       :: Int
    }
  deriving (Show)

arm :: RobotPart
arm =
  RobotPart {name = "arm", description = "super arm", cost = 28, count = 548}

leg :: RobotPart
leg =
  RobotPart
    {name = "leg", description = "super leg", cost = 28 + 36, count = 548 - 293}

hed :: RobotPart
hed =
  RobotPart
    { name = "head"
    , description = "super head"
    , cost = 28 + 36 - 21
    , count = 548 - 293 + 158
    }

ass :: RobotPart
ass =
  RobotPart
    { name = "ass"
    , description = "super ass"
    , cost = 28 + 36 - 21 + 1348
    , count = 548 - 293 + 158 - 284
    }

biba :: RobotPart
biba =
  RobotPart
    { name = "biba"
    , description = "super biba"
    , cost = 28 + 36 - 21 + 1348 - 349
    , count = 548 - 293 + 158 - 284 + 3949
    }

robotDB :: M.Map Int RobotPart
robotDB = M.fromList $ zip [1 .. 5] [arm, leg, hed, ass, biba]

main = do
  args <- getArgs
  let [n1, n2] = map read . take 2 $ args :: [Int]
  -- разбираем : надо применить обычную функцию к значению в контексте - применяем <$> functor, потом надо применить обычную функцию многих переменных к значениям в контексте - применяем applicative <*>
  case min <$> (cost <$> M.lookup n1 robotDB) <*> (cost <$> M.lookup n2 robotDB) of
    Nothing -> putStrLn "biba + boba!"
    (Just x) ->
      putStrLn $ mconcat ["minimum is ", show x, ", but still biba + boba."]
