-- Discretize ground sensor detection functions and save to a catalog for modeling in MCDP
module Main where

import Constants
import Data
import Plot
import Types
import Utils

rangeAUBounds :: Bounds Double
rangeAUBounds = mapBounds au rangeBounds

sizeKmBounds :: Bounds Double
sizeKmBounds = mapBounds kilometers sizeBounds

rangeDist :: DistParams -> Dist
rangeDist (q, p) range = p / (1 + q * range ** gamma)

curve :: SensorData -> DistSpread
curve sensor size = (brightnessFactor * sizeFactor ** gamma, coverageRatio)
 where
  horizon :: PositiveReal
  horizon = limit sensor

  brightnessFactor :: PositiveReal
  brightnessFactor = exp (-(horizon / logisticK))

  sizeFactor :: PositiveReal
  sizeFactor = horizonScaling / size

  coverageRatio :: UnitInterval
  coverageRatio = (sterradians . coverage) sensor / (4 * pi)

detectionProbability :: SensorData -> Length -> Length -> Probability
detectionProbability sensor size range = rangeDist (curve sensor (kilometers size)) (au range)

sensorToDistribution :: SensorData -> Dist2D
sensorToDistribution sensor (sizeKm, rangeAU) = detectionProbability sensor (Kilometer sizeKm) (AU rangeAU)

parameterizedSensorDistribution :: SensorData -> Dist
parameterizedSensorDistribution sensor ratio = detectionProbability sensor (Kilometer 1) (AU ratio)

sensorDistributions :: [Dist2D]
sensorDistributions = map sensorToDistribution sensors

sensorHeatmap :: SensorData -> Heatmap
sensorHeatmap sensor =
  heatmap rangeAUBounds sizeKmBounds rangeSamples sizeSamples (sensorToDistribution sensor)

sensorHeatmaps :: [Heatmap]
sensorHeatmaps = map sensorHeatmap sensors

rangeToSizeBounds :: Bounds Double
rangeToSizeBounds = ratioBounds rangeAUBounds sizeKmBounds

ratioSampled :: [Double]
ratioSampled = map (linspaceToRatio rangeAUBounds sizeKmBounds) $ linspace ratioSamples (0, 1)

evalSamples :: SensorData -> [Double]
evalSamples sensor = map (parameterizedSensorDistribution sensor) ratioSampled

evalSamplesCombined :: SensorData -> Int -> [Double]
evalSamplesCombined sensor i = map (\x -> 1 - (1 - x) ^ i) $ evalSamples sensor

sensorSamples :: [[Double]]
sensorSamples = map evalSamples sensors

sensorOption :: SensorData -> Int -> Text
sensorOption sensor i = addOption functionalitySamples (sensorName sensor <> show i) costs
 where
  costs :: [Text]
  costs =
    map (\x -> show x <> space <> costUnit) [i * fixedCost sensor, i * recurringCost sensor]

  functionalitySamples :: [Text]
  functionalitySamples = map show $ evalSamplesCombined sensor i

sensorResources :: [Text]
sensorResources =
  [ addResource fixedCostName costUnit
  , addResource recurringCostName costUnit
  ]

sensorFunctionalities :: [Text]
sensorFunctionalities = addFunctionalities sensorDetectionFunctionName dimensionlessUnit ratioSamples

makeSensorCatalog :: SensorData -> Text
makeSensorCatalog sensor = makeCatalog catalogBody
 where
  catalogBody :: [Text]
  catalogBody =
    sensorFunctionalities ++ [emptyLine] ++ sensorResources ++ [emptyLine] ++ options sensor

  options :: SensorData -> [Text]
  options sensor = map (sensorOption sensor) [1 .. costOptions]

sensorCatalogs :: [(Text, Text)]
sensorCatalogs = zip (map sensorName sensors) (map makeSensorCatalog sensors)

interface :: Text
interface = makeInterface $ sensorResources ++ [emptyLine] ++ sensorFunctionalities

probabilitySpace :: [(Double, Double)]
probabilitySpace = concat $ diag linearSamples
 where
  linearSamples :: [Double]
  linearSamples = linspace probabilitySamples (0, 1)

probabilityImplementations :: [Text]
probabilityImplementations = zipWith implementation [1 ..] probabilitySpace
 where
  implementation :: Int -> (Double, Double) -> Text
  implementation i (x, y) = addOption [show (fuse x y)] (probabilityName <> show i) (map show [x, y])

  fuse :: Double -> Double -> Double
  fuse x y = 1 - (1 - x) * (1 - y)

probabilityCatalog :: Text
probabilityCatalog =
  makeCatalog $
    resources ++ [emptyLine] ++ functionalities ++ [emptyLine] ++ probabilityImplementations
 where
  resources :: [Text]
  resources =
    [ addResource p1Name dimensionlessUnit
    , addResource p2Name dimensionlessUnit
    ]

  functionalities :: [Text]
  functionalities = [addFunctionality pName dimensionlessUnit]

writeCatalog :: (Text, Text) -> IO ()
writeCatalog (name, catalog) = writeFileText (root </> sensorsLib </> toString name ++ extension) catalog

writeSensorCatalogs :: IO ()
writeSensorCatalogs = mapM_ writeCatalog sensorCatalogs

writeSensorInterface :: IO ()
writeSensorInterface =
  writeFileText
    (root </> sensorsLib </> sensorInterfaceName ++ interfaceExtension)
    interface

writeProbabilityCatalog :: IO ()
writeProbabilityCatalog =
  writeFileText
    (root </> sensorsLib </> probabilityCatalogName ++ extension)
    probabilityCatalog

main :: IO ()
main = do
  writeSensorCatalogs
  writeSensorInterface
  writeProbabilityCatalog

-- main = mapM_ plot sensorHeatmaps
