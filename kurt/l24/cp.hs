{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment
import           System.IO

main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "Wrong arguments!"
    else do
      let [f1, f2] = args
      input <- TIO.readFile f1
      TIO.writeFile f2 input
