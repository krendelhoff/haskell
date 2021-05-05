import           Data.Char
import           Data.List

data Error
  = ParsingError
  | IncompleteDataError
  | IncorrectDataError String
  deriving (Show)

data Person =
  Person
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    }
  deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson s
  | any (not . checkParse) lineS || null s = Left ParsingError
  | not allFields = Left IncompleteDataError
  | not allNum = Left $ IncorrectDataError ageValue
  | otherwise =
    Right
      (Person
         {firstName = firstNameValue, lastName = lastNameValue, age = ageInt})
  where
    label line = takeWhile (\x -> x /= ' ') line
    value line = drop 3 . dropWhile (\x -> x /= ' ') $ line
    checkParse s =
      let lab = label s
          val = value s
       in lab ++ " = " ++ val == s && (not . null $ val) && (not . null $ lab)
    lineS = lines s
    completeList = ["age", "firstName", "lastName"]
    assocS = map (\x -> (label x, value x)) lineS
    allFields =
      let lst = map fst assocS
       in and (foldr (\x acc -> (x `elem` lst) : acc) [] completeList)
    ageValue = snd . head . filter (\(x, _) -> x == "age") $ assocS
    lastNameValue = snd . head . filter (\(x, _) -> x == "lastName") $ assocS
    firstNameValue = snd . head . filter (\(x, _) -> x == "firstName") $ assocS
    allNum = all isDigit ageValue
    ageInt = (read ageValue) :: Int

main = do
  interact (\str -> show $ parsePerson str)
