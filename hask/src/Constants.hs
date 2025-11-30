module Constants where

import GHC.Float (log)

import Types

-- discretization settings
rangeSamples :: Int
rangeSamples = 10

sizeSamples :: Int
sizeSamples = 10

ratioSamples :: Int
ratioSamples = 3

costOptions :: Int
costOptions = 10

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

-- file paths
fileSep :: String
fileSep = "/"

extension :: String
extension = ".mcdp"

interfaceExtension :: String
interfaceExtension = ".mcdp_interface"

root :: String
root = ".."

sensorsLib :: String
sensorsLib = "sensors.mcdplib"

sensorInterfaceName :: String
sensorInterfaceName = "SensorInterface"

-- delimiter for in-line replacements
delim :: Text
delim = toText "%"

-- mcdp syntax
startCatalog :: Text
startCatalog = "catalog {"

endCatalog :: Text
endCatalog = "}"

startInterface :: Text
startInterface = "interface {"

endInterface :: Text
endInterface = "}"

emptyLine :: Text
emptyLine = ""

newline :: Text
newline = "\n"

space :: Text
space = " "

listSep :: Text
listSep = "," <> space

lbrack :: Text
lbrack = "["

rbrack :: Text
rbrack = "]"

indentation :: Text
indentation = toText $ replicate 2 ' '

startFunctionality :: Text
startFunctionality = "provides"

startResource :: Text
startResource = "requires"

optionProvides :: Text
optionProvides = "↤"

optionRequires :: Text
optionRequires = "↦"

sensorDetectionFunctionName :: Text
sensorDetectionFunctionName = "d"

dimensionlessUnit :: Text
dimensionlessUnit = "dimensionless"

fixedCostName :: Text
fixedCostName = "fixedCost"

recurringCostName :: Text
recurringCostName = "recurringCost"

costUnit :: Text
costUnit = "USD"
