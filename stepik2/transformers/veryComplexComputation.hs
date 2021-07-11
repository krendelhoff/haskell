import           Data.Bifunctor
import           Data.Char      (toUpper)
import           Data.List
import           MyRWT

logFirstAndRetSecond :: MyRWT Maybe String
logFirstAndRetSecond = do
  xs <- myAsk
  case xs of
    (el1:el2:_) -> myTell el1 >> return (map toUpper el2)
    _           -> myLift Nothing

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  s1 <- myWithReader (filter $ even . length) logFirstAndRetSecond
  myTell ","
  s2 <- myWithReader (filter $ odd . length) logFirstAndRetSecond
  return (s1, s2)
