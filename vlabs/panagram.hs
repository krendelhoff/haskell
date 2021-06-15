import           Data.Char

isPanagram :: String -> Bool
isPanagram s =
  let correctS = map toLower s
   in and . map (`elem` correctS) $ ['a' .. 'z']
