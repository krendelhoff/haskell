allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f k = pure f <*> k

example :: Maybe Int
example = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

packVars = [6, 12]

lastNightVars = ((\x -> x - 2) . (\x -> x - 2)) <$> packVars

friends = [2, 3]

friendsDrink = [3, 4]

averageBeerAfterNight = (*) <$> friends <*> friendsDrink

needToBuy = (-) <$> averageBeerAfterNight <*> lastNightVars
-- non determinism, all possible variants
