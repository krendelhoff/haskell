{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString    as B
import           Data.Maybe
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO       as TIO

type Author = T.Text

type Title = T.Text

type Html = T.Text

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

type MarcDirectoryRaw = B.ByteString

type MarcDirectoryEntryRaw = B.ByteString

type FieldText = T.Text

data Book =
  Book
    { author :: Author
    , title  :: Title
    }
  deriving (Show)

data FieldMetadata =
  FieldMetadata
    { tag         :: T.Text
    , fieldLength :: Int
    , fieldStart  :: Int
    }
  deriving (Show)

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n"
    , "<head><title>books</title>"
    , "<meta charset='utf-8'/>"
    , "</head>\n"
    , "<body>\n"
    , booksHtml
    , "\n</body>\n"
    , "</html>"
    ]
  where
    booksHtml = (mconcat . (map bookToHtml)) books

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where
    recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream =
  if B.null marcStream
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest marcStream

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where
    remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if B.null directory
    then []
    else nextEntry : splitDirectory restEntries
  where
    (nextEntry, restEntries) = B.splitAt dirEntryLength directory

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where
    (theTag, rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 theTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

getFieldMetaData :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetaData = map makeFieldMetadata

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if length results < 1
    then Nothing
    else Just (head results)
  where
    metadata = getFieldMetaData . splitDirectory . getDirectory $ record
    results = filter ((== aTag) . tag) metadata

-- тут происходит вычисление с контекстом - функция выше есть стрелка клейсли, используя монаду Maybe можно было бы избежать этого костыля внизу(то, что мы принимаем Maybe, а не просто FieldMetadata, скомпозировать их через >>= (bind)
lookupSubfield :: FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield fieldMetadata subfield record =
  if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = do
  entryMetadata <- lookupFieldMetadata aTag record
  lookupSubfield entryMetadata subfield record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupFullTitle :: MarcRecordRaw -> Maybe Title
lookupFullTitle record = do
  title <- lookupTitle record
  case lookupValue titleTag 'b' record of
    Nothing         -> return title
    (Just extended) -> return (title <> " " <> extended)

-- тут тоже явно можно сделать функцию высшего порядка, паттерн очевиден, для вычленения любого поля
lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

marcToFullPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToFullPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupFullTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
  map
    (\(title, author) -> Book {title = fromJust title, author = fromJust author})
    justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

processRecordsExtended :: Int -> B.ByteString -> Html
processRecordsExtended n = booksToHtml . pairsToBooks . take n . marcToFullPairs

main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecordsExtended 500 marcData
  TIO.writeFile "books.html" processed
  TIO.writeFile "books+boba.html" $ processRecords 500 marcData
