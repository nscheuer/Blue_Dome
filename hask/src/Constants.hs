module Constants where

import GHC.Float (log)

import Types

-- discretization settings
rangeSamples :: Int
rangeSamples = 3

sizeSamples :: Int
sizeSamples = 3

velocitySamples :: Int
velocitySamples = 3

ratioSamples :: Int
ratioSamples = 3

probabilitySamples :: Int
probabilitySamples = 3

costOptions :: Int
costOptions = 3

representativeTimes :: [Time]
representativeTimes = [Day 1, Day 10, Day 100]

rangeBounds :: Bounds Length
rangeBounds = (AU 0, AU 5)

sizeBounds :: Bounds Length
sizeBounds = (Kilometer 0.02, Kilometer 100)

velocityBounds :: Bounds Velocity
velocityBounds = ((Kilometer 0, Second 1), (Kilometer 60, Second 1))

-- sensor parameters
logisticK :: PositiveReal
logisticK = 0.7

brightnessScaling :: PositiveReal
brightnessScaling = 5 / log 10

gamma :: PositiveReal
gamma = brightnessScaling / logisticK

horizonScaling :: PositiveReal
horizonScaling = 1329 / sqrt 0.14

-- asteroid distribution parameters
sizeDistB :: PositiveReal
sizeDistB = 1.35

-- PDF generator where x axis is km/s and y axis is relative probability
velocityBins :: NonEmpty (PositiveReal, Probability)
velocityBins =
  (0, 0)
    :| [ (5, 5e-3)
       , (10, 0.03)
       , (15, 0.06)
       , (20, 0.08)
       , (25, 0.07)
       , (30, 0.06)
       , (35, 0.035)
       , (40, 0.02)
       , (45, 0.01)
       , (50, 0)
       , (55, 0)
       , (60, 0)
       ]

-- unit conversion
sterradiansPerSquareDegree :: PositiveReal
sterradiansPerSquareDegree = pi ^ 2 / 180 ^ 2

metersPerKilometer :: PositiveReal
metersPerKilometer = 1e3

metersPerAU :: PositiveReal
metersPerAU = 1.496e11

secondsPerDay :: PositiveReal
secondsPerDay = 86400

daysPerYear :: PositiveReal
daysPerYear = 365

-- file paths
fileSep :: String
fileSep = "/"

underscore :: String
underscore = "_"

extension :: String
extension = ".mcdp"

interfaceExtension :: String
interfaceExtension = ".mcdp_interface"

root :: String
root = ".."

sensorsLib :: String
sensorsLib = "sensors.mcdplib"

detectionLib :: String
detectionLib = "detection.mcdplib"

detectionName :: String
detectionName = "detection"

sensorInterfaceName :: String
sensorInterfaceName = "SensorInterface"

probabilityCatalogName :: String
probabilityCatalogName = "probability"

selectionName :: String
selectionName = "selection"

sensorSelectionName :: String
sensorSelectionName = "selection"

sensorSelectionRelPath :: String
sensorSelectionRelPath = "sensors." ++ selectionName

timeToImpactReductionName :: String
timeToImpactReductionName = "time_to_impact_reduction"

-- delimiter for in-line replacements
delim :: Text
delim = "%"

-- mcdp syntax
startMCDP :: Text
startMCDP = "mcdp {"

endMCDP :: Text
endMCDP = "}"

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

blank :: [Text]
blank = [emptyLine]

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

equals :: Text
equals = "="

ge :: Text
ge = ">="

le :: Text
le = "<="

plusSign :: Text
plusSign = "+"

timesSign :: Text
timesSign = "*"

backtick :: Text
backtick = "`"

indentation :: Text
indentation = toText $ replicate 2 ' '

startFunctionality :: Text
startFunctionality = "provides"

startResource :: Text
startResource = "requires"

startSub :: Text
startSub = "sub"

instanceKeyword :: Text
instanceKeyword = "instance"

requiredKeyword :: Text
requiredKeyword = "required"

providedKeyword :: Text
providedKeyword = "provided"

forKeyword :: Text
forKeyword = "for"

usingKeyword :: Text
usingKeyword = "using"

requiredBy :: Text
requiredBy = "required by"

providedBy :: Text
providedBy = "provided by"

optionProvides :: Text
optionProvides = "↤"

optionRequires :: Text
optionRequires = "↦"

sensorDetectionFunctionName :: Text
sensorDetectionFunctionName = "d"

probabilityName :: Text
probabilityName = "p"

dimensionlessUnit :: Text
dimensionlessUnit = "dimensionless"

fixedCostName :: Text
fixedCostName = "fixed_cost"

recurringCostName :: Text
recurringCostName = "recurring_cost"

p1Name :: Text
p1Name = "x"

p2Name :: Text
p2Name = "y"

pName :: Text
pName = "z"

timeToImpactName :: Text
timeToImpactName = "time_to_impact"

costUnit :: Text
costUnit = "USD"
