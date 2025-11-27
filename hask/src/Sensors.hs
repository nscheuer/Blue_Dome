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

sensorSamples :: [[Double]]
sensorSamples = map evalSamples sensors

-- todo: write sensor samples to sensor files as yaml catalogs

main :: IO ()
main = mapM_ plot sensorHeatmaps
