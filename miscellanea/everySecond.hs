-- pattern matching - очень гибкая штука, вообще всё можно мэтчить, синглетоны например

everySecond :: [a] -> [a]
everySecond [] = []
everySecond [x] = []
everySecond (x:y:xs) = y:(everySecond xs)

main = print (everySecond "pdpottiay2")
