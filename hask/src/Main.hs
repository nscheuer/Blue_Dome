-- Discretize ground sensor detection functions and save to a catalog for modeling in MCDP
module Main where

import Constants
import Data
import Plot
import Types
import Utils

import System.Directory (renameFile)

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

combineSensors :: SensorData -> SensorData -> Heatmap
combineSensors sensor1 sensor2 =
  heatmap
    rangeAUBounds
    sizeKmBounds
    rangeSamples
    sizeSamples
    (\(x, y) -> 1 - (1 - p1 (x, y)) * (1 - p2 (x, y)))
 where
  p1 :: Dist2D
  p1 = sensorToDistribution sensor1

  p2 :: Dist2D
  p2 = sensorToDistribution sensor2

rangeToSizeBounds :: Bounds Double
rangeToSizeBounds = ratioBounds rangeAUBounds sizeKmBounds

ratioSampled :: [Double]
ratioSampled = map (linspaceToRatio rangeAUBounds sizeKmBounds) $ linspace ratioSamples (0, 1)

enumeratedRatios :: [(Int, Double)]
enumeratedRatios = zip [1 ..] ratioSampled

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
  functionalitySamples = map showDec $ evalSamplesCombined sensor i

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
    sensorFunctionalities ++ blank ++ sensorResources ++ blank ++ options sensor

  options :: SensorData -> [Text]
  options sensor = map (sensorOption sensor) [1 .. costOptions]

sensorCatalogs :: [(Text, Text)]
sensorCatalogs = zip (map sensorName sensors) (map makeSensorCatalog sensors)

interface :: Text
interface = makeInterface $ sensorResources ++ blank ++ sensorFunctionalities

probabilitySpace :: [(Double, Double)]
probabilitySpace = concat $ diag linearSamples
 where
  linearSamples :: [Double]
  linearSamples = linspace probabilitySamples (0, 1)

probabilityImplementations :: [Text]
probabilityImplementations = zipWith implementation [1 ..] probabilitySpace
 where
  implementation :: Int -> (Double, Double) -> Text
  implementation i (x, y) = addOption [showDec (fuse x y)] (probabilityName <> show i) (map showDec [x, y])

  fuse :: Double -> Double -> Double
  fuse x y = 1 - (1 - x) * (1 - y)

probabilityCatalog :: Text
probabilityCatalog =
  makeCatalog $
    resources ++ blank ++ functionalities ++ blank ++ probabilityImplementations
 where
  resources :: [Text]
  resources =
    [ addResource p1Name dimensionlessUnit
    , addResource p2Name dimensionlessUnit
    ]

  functionalities :: [Text]
  functionalities = [addFunctionality pName dimensionlessUnit]

makeSelectionDP :: String -> String -> Text
makeSelectionDP sub1 sub2 =
  makeMCDP
    ( sensorResources
        ++ blank
        ++ sensorFunctionalities
        ++ blank
        ++ probabilityInstances
        ++ blank
        ++ instances
        ++ blank
        ++ costReq
        ++ blank
        ++ funProv
    )
 where
  s1 :: Text
  s1 = toText sub1

  s2 :: Text
  s2 = toText sub2

  probabilityInstances :: [Text]
  probabilityInstances =
    map
      (\i -> addInstance (probabilityName <> show i) (toText probabilityCatalogName))
      [1 .. ratioSamples]

  instances :: [Text]
  instances = map addSingleInstance [s1, s2]

  costReq :: [Text]
  costReq =
    [ required fixedCostName
        `greaterThan` ((fixedCostName `reqBy` s1) `plus` (fixedCostName `reqBy` s2))
    , required recurringCostName
        `greaterThan` ((recurringCostName `reqBy` s1) `plus` (recurringCostName `reqBy` s2))
    ]

  funProv :: [Text]
  funProv = concatMap funProvSample [1 .. ratioSamples]

  funProvSample :: Int -> [Text]
  funProvSample i =
    let
      prob :: Text
      prob = probabilityName <> show i

      sample :: Text
      sample = sensorDetectionFunctionName <> show i
     in
      [ (p1Name `reqBy` prob) `lessThan` (sample `provBy` s1)
      , (p2Name `reqBy` prob) `lessThan` (sample `provBy` s2)
      , provided sample `lessThan` (pName `provBy` prob)
      ]

detection :: Text
detection =
  makeMCDP
    (instances ++ blank ++ resources ++ blank ++ functionalities ++ blank ++ constraints)
 where
  sensorSelection :: Text
  sensorSelection = toText sensorSelectionName

  sensorSelectionModule :: Text
  sensorSelectionModule = toText sensorSelectionRelPath

  timeToImpactReduction :: Text
  timeToImpactReduction = toText timeToImpactReductionName

  instances :: [Text]
  instances =
    [ addInstance sensorSelection sensorSelectionModule
    , addSingleInstance timeToImpactReduction
    ]

  resources :: [Text]
  resources =
    [ requiresFor fixedCostName sensorSelection
    , requiresFor recurringCostName sensorSelection
    ]

  functionalities :: [Text]
  functionalities =
    map
      (\i -> providesUsing (timeToImpactName <> show i) timeToImpactReduction)
      [1 .. length representativeTimes]

  constraints :: [Text]
  constraints = map makeConstraint [1 .. ratioSamples]

  makeConstraint :: Int -> Text
  makeConstraint i =
    let
      sample :: Text
      sample = sensorDetectionFunctionName <> show i
     in
      (sample `reqBy` timeToImpactReduction) `lessThan` (sample `provBy` sensorSelection)

timeToImpactReduction :: Text
timeToImpactReduction = makeMCDP (resources ++ blank ++ functionalities ++ blank ++ constraints)
 where
  resources :: [Text]
  resources = addResources sensorDetectionFunctionName dimensionlessUnit ratioSamples

  functionalities :: [Text]
  functionalities = addFunctionalities timeToImpactName dimensionlessUnit (length representativeTimes)

  constraints :: [Text]
  constraints = zipWith makeConstraint [1 .. length representativeTimes] representativeTimes

  makeConstraint :: Int -> Time -> Text
  makeConstraint i time = provided (timeToImpactName <> show i) `lessThan` hazardIntegral (seconds time)

  hazardIntegral :: Double -> Text
  hazardIntegral t = mcdpSum (map (sizeIntegral t) $ linspace sizeSamples (mapBounds kilometers sizeBounds))

  sizeIntegral :: Double -> Double -> Text
  sizeIntegral t s =
    mcdpSum
      ( map (velocityIntegral t s) $
          linspace velocitySamples (mapBounds kilometersPerSecond velocityBounds)
      )

  velocityIntegral :: Double -> Double -> Double -> Text
  velocityIntegral t s v =
    let
      rhoV :: Double
      rhoV = velocityPDF (Kilometer v, Second 1)

      rhoS :: Double
      rhoS = sizePDF (Kilometer s)
     in
      required (sensorDetectionFunctionName <> show (fIndex t s v))
        `times` showDec rhoV
        `times` showDec rhoS

  -- find index between 1 and `ratioSamples` corresponding to the closest range/size ratio variable for calculating detection probability
  -- `t` in seconds, `s` in km, `v` in km/s; but queried ratio is taken as range in AU divided by size in km
  fIndex :: Double -> Double -> Double -> Int
  fIndex t s v = let ratio = au (Kilometer (v * t)) / s in findClosestIndex ratio enumeratedRatios

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

writeSelectionPair :: Int -> (Int, (String, String)) -> IO String
writeSelectionPair i (j, (sub1, sub2)) = do
  let name = selectionName ++ show i ++ underscore ++ show j
  writeFileText (root </> sensorsLib </> name ++ extension) (makeSelectionDP sub1 sub2)
  return name

writeSelection :: Int -> [String] -> IO (Maybe String)
writeSelection i subproblems =
  if length subproblems == 1
    then return (viaNonEmpty head subproblems)
    else do
      next <- written
      writeSelection (i + 1) (next ++ remaining)
 where
  tasks :: ([(String, String)], [String])
  tasks = pair subproblems

  remaining :: [String]
  remaining = snd tasks

  -- group subproblems into disjoint pairs and enumerate them
  pairs :: [(Int, (String, String))]
  pairs = zip [1 ..] (fst tasks)

  -- write relevant DPs for combining subproblems
  written :: IO [String]
  written = mapM (writeSelectionPair i) pairs

writeSensorSelection :: IO ()
writeSensorSelection = do
  block <- writeSelection 1 (map (toString . sensorName) sensors)
  case block of
    Just name ->
      renameFile
        (root </> sensorsLib </> name ++ extension)
        (root </> sensorsLib </> selectionName ++ extension)
    Nothing -> putStrLn "No selection written"

writeDetection :: IO ()
writeDetection = writeFileText (root </> detectionLib </> detectionName ++ extension) detection

writeTimeToImpact :: IO ()
writeTimeToImpact =
  writeFileText
    (root </> detectionLib </> timeToImpactReductionName ++ extension)
    timeToImpactReduction

main :: IO ()
main = do
  writeSensorCatalogs
  writeSensorInterface
  writeProbabilityCatalog
  writeSensorSelection
  writeDetection
  writeTimeToImpact

-- selectedSensorHeatmaps :: [Heatmap]
-- selectedSensorHeatmaps = [sensorHeatmap ztf, sensorHeatmap rubin, combineSensors ztf rubin]

-- main = mapM_ plot selectedSensorHeatmaps
