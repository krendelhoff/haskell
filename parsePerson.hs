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
  | any (not . checkParse) lineS = Left ParsingError
  | not allFields = Left IncompleteDataError
  | not allNum = Left (IncorrectDataError ageValue)
  | otherwise =
    Right
      (Person
         {firstName = firstNameValue, lastName = lastNameValue, age = ageInt})
  where
    checkParse s = (unwords . words $ s) == s
    lineS = lines s
    completeList = ["age", "firstName", "lastName"]
    wordS = map words lineS
    assocS = map (\x -> (head x, last x)) wordS
    allFields = ((map fst assocS) \\ completeList) == []
    ageValue = snd . head . filter (\(x, _) -> x == "age") $ assocS
    lastNameValue = snd . head . filter (\(x, _) -> x == "lastName") $ assocS
    firstNameValue = snd . head . filter (\(x, _) -> x == "firstName") $ assocS
    allNum = all isDigit ageValue
    ageInt = (read ageValue) :: Int

main = do
  interact (\str -> show $ parsePerson str)
