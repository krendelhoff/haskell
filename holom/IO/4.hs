import           Control.Monad
import           Data.Foldable
import           Data.List
import           System.Environment
import           System.IO

-- второй вариант как обычно - накопить чисто, через fmt красиво вывести
func :: String -> Handle -> IO ()
func wordString handle = do
  let wordList = words wordString
  fileLines <- lines <$> hGetContents handle
  traverse_
    (\word ->
       traverse_
         (\line -> do when (word `isInfixOf` line) (putStrLn line))
         fileLines)
    wordList

main = do
  [wordString, filePath] <- getArgs
  withFile filePath ReadMode $ func wordString
