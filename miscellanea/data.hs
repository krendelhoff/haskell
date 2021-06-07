data Geometry = Rectangle Double Double
              | Square Double
              | Circle Double
              deriving (Show, Eq, Ord, Read)

main :: IO ()
main = print (read (show $ Circle 5) :: Geometry)
-- пока не очень понятно зачем явно указывать тип, ведь нет другого типа разделяющего этот конструктор
-- ну или понятно, зачем ему проверять всё на совпадение, нужно просто чтобы мы указали
