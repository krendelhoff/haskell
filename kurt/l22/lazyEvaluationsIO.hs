main = do
  rawInput <- getContents
  let input = map (read :: String -> Int) . lines $ rawInput
  mapM_ print $ fst (break (== 128) input)
