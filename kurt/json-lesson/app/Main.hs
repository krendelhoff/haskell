module Main where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy       as B
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  as T
import           GHC.Generics

data IntList
  = EmptyList
  | Cons Int IntList
  deriving (Show, Generic)

instance ToJSON IntList

data Book =
  Book
    { title  :: T.Text
    , author :: T.Text
    , year   :: Int
    }
  deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

data NOAAResult =
  NOAAResult
    { uid, mindate, maxdate, name, resultID :: T.Text
    , datacoverage                          :: Double
    }
  deriving (Show)

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid" <*> v .: "mindate" <*> v .: "maxdate" <*>
    v .: "name" <*>
    v .: "id" <*>
    v .: "datacoverage"

data Resultset =
  Resultset
    { offset, count, limit :: Int
    }
  deriving (Show, Generic)

instance FromJSON Resultset

data Metadata =
  Metadata
    { resultset :: Resultset
    }
  deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse =
  NOAAResponse
    { metadata :: Metadata
    , results  :: [NOAAResult]
    }
  deriving (Show, Generic)

instance FromJSON NOAAResponse

instance ToJSON NOAAResponse where
  toJSON (NOAAResponse {metadata = meta, results = res}) =
    object $ ["metadata" .= toJSON meta, "results" .= toJSON res]

instance ToJSON Metadata where
  toJSON (Metadata result) = object ["resultset" .= toJSON result]

instance ToJSON Resultset where
  toJSON (Resultset offset count limit) =
    object ["offset" .= offset, "count" .= count, "limit" .= limit]

instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name resultID datacoverage) =
    object
      [ "uid" .= uid
      , "mindate" .= mindate
      , "maxdate" .= maxdate
      , "name" .= name
      , "datacoverage" .= datacoverage
      , "id" .= resultID
      ]

printResults :: Either String [NOAAResult] -> IO ()
printResults (Left boba) = print boba
printResults (Right results) = do
  forM_ results (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
      noaaResults = results <$> noaaResponse
  B.writeFile "new.json" ((\(Right x) -> encode x) noaaResponse)
  B.writeFile "list.json" (encode (Cons 1 (Cons 2 (Cons 3 EmptyList))))
  printResults noaaResults
