type Boba = String

f :: Boba -> Bool
f boba = True

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  print (f "wow")
-- он не выдал ошибку и вывел тип
  print ((\(Just a) -> a) (Just 5))
--  print (safeHead [])
    -- выдает ошибку, т.к. это обозначение для пустого списка любого типа
    -- но возвращает то он Maybe a, то есть тип конкретный, если не указать, то там ambiguous
  print (safeHead ([] :: [Int]))
    -- так же обсолютно очев. почему если derive read, то всегда надо явно указывать - откуда он знает, надо считать просто строку, или там конструктор?"
