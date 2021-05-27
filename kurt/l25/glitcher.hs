import           Control.Monad
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           System.Environment
import           System.Random

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]
  where
    intToChar int = toEnum $ int `mod` 255

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.tail rest
    newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  sectionSize <- randomRIO (25, 40)
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

type From = Int

type Size = Int

reverseBytes :: From -> Size -> BC.ByteString -> BC.ByteString
reverseBytes start size bytes = mconcat [before, reversed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    reversed = BC.reverse target

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
  sectionSize <- randomRIO (25, 40)
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (reverseBytes start sectionSize bytes)

-- тут можно увидеть паттерн: randomReverseBytes и randomSortSection идентичны с точностью до совершаемого действия и действия тоже идентичны с точностью до примененной к target функции - можно написать функцию высшего порядка и сделать обе эти частичным применением
glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReverseBytes]
  {-[ randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  ]-}

main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"
