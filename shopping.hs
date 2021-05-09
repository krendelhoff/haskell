newtype Writer w a =
  Writer
    { runWriter :: (a, w)
    }

execWriter :: Writer w a -> w
execWriter = snd . runWriter

type Shopping = Writer ([String], Integer) ()

purchase :: String -> Integer -> Shopping
purchase item cost = Writer ((), ([item], cost))

total :: Shopping -> Integer
total = snd . execWriter

items :: Shopping -> [String]
items x = fst . execWriter
