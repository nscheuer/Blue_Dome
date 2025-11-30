module Utils where

import Constants
import Types

import Data.Foldable (minimumBy)
import Data.Text as T (intercalate, replace)

-- unit conversion
squareDegrees :: SolidAngle -> Double
squareDegrees (SquareDegree x) = x
squareDegrees (Steradian x) = x / sterradiansPerSquareDegree

sterradians :: SolidAngle -> Double
sterradians (SquareDegree x) = x * sterradiansPerSquareDegree
sterradians (Steradian x) = x

meters :: Length -> Double
meters (Meter x) = x
meters (Kilometer x) = x * metersPerKilometer
meters (AU x) = x * metersPerAU

kilometers :: Length -> Double
kilometers (Meter x) = x / metersPerKilometer
kilometers (Kilometer x) = x
kilometers (AU x) = kilometers (Meter (x * metersPerAU))

au :: Length -> Double
au (Meter x) = x / metersPerAU
au (Kilometer x) = au (Meter (x * metersPerKilometer))
au (AU x) = x

seconds :: Time -> Double
seconds (Second x) = x
seconds (Day x) = x * secondsPerDay
seconds (Year x) = x * (daysPerYear * secondsPerDay)

days :: Time -> Double
days (Second x) = x / secondsPerDay
days (Day x) = x
days (Year x) = days (Second (x * daysPerYear * secondsPerDay))

years :: Time -> Double
years (Second x) = x / (daysPerYear * secondsPerDay)
years (Day x) = x / daysPerYear
years (Year x) = x

metersPerSecond :: Velocity -> Double
metersPerSecond (l, t) = meters l / seconds t

kilometersPerSecond :: Velocity -> Double
kilometersPerSecond (l, t) = kilometers l / seconds t

-- distributions
sizePDF :: PDF Length
sizePDF size
  | s <= smin = 0
  | s >= smax = 1
  | otherwise = c * s ** (-(sizeDistB + 1))
 where
  s :: Double
  s = kilometers size

  c :: Double
  c = sizeDistB / (smin ** (-sizeDistB) - smax ** (-sizeDistB))

  smin :: Double
  smin = (kilometers . fst) sizeBounds

  smax :: Double
  smax = (kilometers . snd) sizeBounds

lerp :: UnitInterval -> Double -> Double -> Double
lerp t a b = a + t * (b - a)

interpolate :: Double -> [(Double, Double)] -> Double
interpolate x xs = case xs of
  [] -> 0
  [(x1, y1)] -> y1
  (x1, y1) : (x2, y2) : rest ->
    if x < x1
      then y1
      else if x >= x2 then interpolate x rest else lerp ((x - x1) / (x2 - x1)) y1 y2

velocityPDF :: PDF Velocity
velocityPDF velocity = interpolate v (toList velocityBins) / area
 where
  v :: Double
  v = kilometersPerSecond velocity

  area :: Double
  area =
    sum $
      zipWith
        (\(x1, y1) (x2, y2) -> (x2 - x1) * (y1 + y2) / 2)
        (toList velocityBins)
        (tail velocityBins)

-- type wrangling
mapBounds :: (a -> b) -> Bounds a -> Bounds b
mapBounds f (start, end) = (f start, f end)

-- sampling
linspace :: Int -> Bounds Double -> [Double]
linspace n (start, end) = [start, start + step .. end]
 where
  step = (end - start) / (fromIntegral n - 1)

stitch :: [Double] -> [Double] -> [[(Double, Double)]]
stitch xs ys = [[(x, y) | y <- ys] | x <- xs]

diag :: [Double] -> [[(Double, Double)]]
diag xs = stitch xs xs

broadcast :: ((Double, Double) -> a) -> [[(Double, Double)]] -> Grid a
broadcast f array = [[((x, y), f (x, y)) | (x, y) <- row] | row <- array]

grid :: Bounds Double -> Bounds Double -> Int -> Int -> ((Double, Double) -> a) -> Grid a
grid (xmin, xmax) (ymin, ymax) nx ny f = broadcast f (stitch (linspace nx (xmin, xmax)) (linspace ny (ymin, ymax)))

heatmap :: Bounds Double -> Bounds Double -> Int -> Int -> Dist2D -> Grid Probability
heatmap (xmin, xmax) (ymin, ymax) nx ny f = grid (xmin, xmax) (ymin, ymax) nx ny (boundByZero . f)
 where
  -- replace NaN with zero
  boundByZero :: Double -> Probability
  boundByZero x = if isNaN x then 0 else x

ratioBounds :: Bounds Double -> Bounds Double -> Bounds Double
ratioBounds (xmin, xmax) (ymin, ymax) = (xmin / ymax, xmax / ymin)

linspaceToRatio :: Bounds Double -> Bounds Double -> UnitInterval -> Double
linspaceToRatio (xmin, xmax) (ymin, ymax) t = (xmin + (xmax - xmin) * t) / (ymax + (ymin - ymax) * t)

findClosestIndex :: Double -> [(Int, Double)] -> Int
findClosestIndex x enumeratedList = fst $ minimumBy (comparing snd) [(i, abs (x - r)) | (i, r) <- enumeratedList]

-- task division
pair :: [a] -> ([(a, a)], [a])
pair [] = ([], [])
pair [x] = ([], [x])
pair (x : y : xs) = ((x, y) : paired, rem)
 where
  (paired, rem) = pair xs

-- join paths
(</>) :: String -> String -> String
a </> b = a ++ fileSep ++ b

-- find and replace
hydrate :: Text -> Text -> Text -> Text
hydrate old = replace (delim <> old <> delim)

-- serialize
stringLines :: [Text] -> Text
stringLines = T.intercalate newline

indent :: [Text] -> [Text]
indent = map (indentation <>)

padSpace :: Text -> Text
padSpace t = space <> t <> space

-- interface {...}
makeInterface :: [Text] -> Text
makeInterface lines = stringLines $ [startInterface] ++ indent lines ++ [endInterface, emptyLine]

-- catalog {...}
makeCatalog :: [Text] -> Text
makeCatalog lines = stringLines $ [startCatalog] ++ indent lines ++ [endCatalog, emptyLine]

-- mcdp {...}
makeMCDP :: [Text] -> Text
makeMCDP lines = stringLines $ [startMCDP] ++ indent lines ++ [endMCDP, emptyLine]

-- provides NAME [UNITS]
addFunctionality :: Text -> Text -> Text
addFunctionality name units = startFunctionality <> padSpace name <> lbrack <> units <> rbrack

-- requires NAME [UNITS]
addResource :: Text -> Text -> Text
addResource name units = startResource <> padSpace name <> lbrack <> units <> rbrack

-- sub VARIABLE = instance `NAME
addInstance :: Text -> Text -> Text
addInstance var name = startSub <> padSpace var <> equals <> padSpace instanceKeyword <> backtick <> name

-- sub NAME = instance `NAME
addSingleInstance :: Text -> Text
addSingleInstance name = addInstance name name

-- ...PROVIDES ↤ NAME ↦ ...REQUIRES
-- both sides should include units
addOption :: [Text] -> Text -> [Text] -> Text
addOption provides name requires =
  provided
    <> padSpace optionProvides
    <> name
    <> padSpace optionRequires
    <> required
 where
  provided :: Text
  provided = T.intercalate listSep provides

  required :: Text
  required = T.intercalate listSep requires

addFunctionalities :: Text -> Text -> Int -> [Text]
addFunctionalities name units count = map (\i -> addFunctionality (name <> show i) units) [1 .. count]

addResources :: Text -> Text -> Int -> [Text]
addResources name units count = map (\i -> addResource (name <> show i) units) [1 .. count]

required :: Text -> Text
required name = requiredKeyword <> space <> name

provided :: Text -> Text
provided name = providedKeyword <> space <> name

-- requires NAME for DP
requiresFor :: Text -> Text -> Text
requiresFor name dp = startResource <> padSpace name <> forKeyword <> space <> dp

-- provides NAME using DP
providesUsing :: Text -> Text -> Text
providesUsing name dp = startFunctionality <> padSpace name <> usingKeyword <> space <> dp

reqBy :: Text -> Text -> Text
resource `reqBy` dp = resource <> padSpace requiredBy <> dp

provBy :: Text -> Text -> Text
resource `provBy` dp = resource <> padSpace providedBy <> dp

greaterThan :: Text -> Text -> Text
a `greaterThan` b = a <> padSpace ge <> b

lessThan :: Text -> Text -> Text
a `lessThan` b = a <> padSpace le <> b

plus :: Text -> Text -> Text
a `plus` b = a <> padSpace plusSign <> b

times :: Text -> Text -> Text
a `times` b = a <> padSpace timesSign <> b

mcdpSum :: [Text] -> Text
mcdpSum = T.intercalate (padSpace plusSign)
