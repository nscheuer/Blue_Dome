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

makeSensorCatalog :: SensorData -> Text
makeSensorCatalog sensor = makeCatalog catalogBody
 where
  catalogBody :: [Text]
  catalogBody = functionalities ++ [emptyLine] ++ resources ++ [emptyLine] ++ options sensor

  functionalities :: [Text]
  functionalities = addFunctionalities sensorDetectionFunctionName dimensionlessUnit ratioSamples

  resources :: [Text]
  resources =
    [ addResource fixedCostName costUnit
    , addResource recurringCostName costUnit
    ]

  options :: SensorData -> [Text]
  options sensor = map (sensorOption sensor) [1 .. costOptions]

sensorCatalogs :: [(Text, Text)]
sensorCatalogs = zip (map sensorName sensors) (map makeSensorCatalog sensors)

writeCatalog :: (Text, Text) -> IO ()
writeCatalog (name, catalog) = writeFileText (root </> sensorsLib </> toString name ++ extension) catalog

writeSensorCatalogs :: IO ()
writeSensorCatalogs = mapM_ writeCatalog sensorCatalogs

main :: IO ()
main = writeSensorCatalogs

-- main = mapM_ plot sensorHeatmaps
