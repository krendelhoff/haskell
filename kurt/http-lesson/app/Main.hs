module Main where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Network.HTTP.Simple

myToken :: BC.ByteString
myToken = "sMwjYPMtozVPZONDHGUWUSCmnHhlplfU"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest ::
     BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $
  setRequestPath path $
  setRequestSecure True $ setRequestPort 443 $ defaultRequest

buildRequestNOSSL token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $
  setRequestPath path $
  setRequestSecure False $ setRequestPort 443 $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "POST" apiPath

main :: IO ()
main = do
  response <- httpLBS request
  return ()
  let code = getResponseStatusCode response
      moreInfo = getResponseStatus response
  if code == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else do
      print "request failed with error"
      print "what happened!"
      print moreInfo
