{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO

toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

main = do
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  print (sum numbers)
