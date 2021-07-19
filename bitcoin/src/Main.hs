module Main where

import           Control.Lens          (preview)
import           Control.Monad.Except
import           Data.Aeson.Lens       (_String, key)
import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text)
import qualified Data.Text.IO          as TIO
import           Network.HTTP.Simple   (getResponseBody, httpBS)

fetchJSON :: IO BC.ByteString
fetchJSON = do
  res <- httpBS "https://api.coindesk.com/v1/bpi/currentprice.json"
  return (getResponseBody res)

getRate :: BC.ByteString -> Maybe Text
getRate = preview (key "bpi" . key "USD" . key "rate" . _String)

main :: IO ()
main = do
  json <- fetchJSON
  maybe
    (TIO.putStrLn "Could not find the Bitcoin rate :(")
    (\rate -> TIO.putStrLn $ "The current Bitcoin rate is " <> rate <> "$") $
    getRate json
