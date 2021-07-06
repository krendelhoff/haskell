{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Temperature a =
  Temperature Double
  deriving (Num, Show)

data Celcius

data Fahrenheit

comfortTemperature :: Temperature Celcius
comfortTemperature = Temperature 23

c2f :: Temperature Celcius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)
