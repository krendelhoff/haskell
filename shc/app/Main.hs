module Main where

import           Parser
import           System.Environment

main :: IO ()
main = do
  x <- getLine
  putStrLn $ readExpr x
