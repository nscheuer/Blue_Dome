module Constants where

import GHC.Float (log)

import Types

-- discretization settings
rangeSamples :: Int
rangeSamples = 10

sizeSamples :: Int
sizeSamples = 10

ratioSamples :: Int
ratioSamples = 10

rangeBounds :: Bounds Length
rangeBounds = (AU 0, AU 5)

sizeBounds :: Bounds Length
sizeBounds = (Kilometer 0, Kilometer 5)

-- sensor parameters
logisticK :: Double
logisticK = 0.7

brightnessScaling :: Double
brightnessScaling = 5 / log 10

gamma :: Double
gamma = brightnessScaling / logisticK

horizonScaling :: Double
horizonScaling = 1329 / sqrt 0.14

-- unit conversion
sterradiansPerSquareDegree :: Double
sterradiansPerSquareDegree = pi ^ 2 / 180 ^ 2

metersPerKilometer :: Double
metersPerKilometer = 1e3

metersPerAU :: Double
metersPerAU = 1.496e11

-- delimiter for in-line replacements
delim :: Text
delim = toText "%"
