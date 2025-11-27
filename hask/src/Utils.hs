module Utils where

import Constants
import Types

import Data.Text (replace)

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

-- join paths
(</>) :: String -> String -> String
a </> b = a ++ "/" ++ b

-- find and replace
hydrate :: Text -> Text -> Text -> Text
hydrate old = replace (delim <> old <> delim)
