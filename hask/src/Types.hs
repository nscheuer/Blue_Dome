module Types where

-- constrained variables
type PositiveReal = Double
type UnitInterval = Double
type Probability = UnitInterval

-- aliases
type Dist = Double -> Probability
type Dist2D = (Double, Double) -> Probability

-- (inverse spread (Q), maximum probability)
type DistParams = (PositiveReal, UnitInterval)

-- defines a distribution over dependent variable at each point in an interval
type DistSpread = Double -> DistParams
type Bounds a = (a, a)
type Cell a = ((Double, Double), a)
type Grid a = [[Cell a]]
type Matrix a = [[a]]
type Heatmap = Grid Probability

-- units
data SolidAngle = SquareDegree Double | Steradian Double
data Length = Meter Double | Kilometer Double | AU Double

-- sensor data and other statistics
data SensorData = SensorData
  { survey :: String
  , limit :: Double -- minimum brightness that can be detected
  , coverage :: SolidAngle -- per night
  -- costs are in USD
  , fixedCost :: Integer
  , recurringCost :: Integer -- annual cost of maintenance
  }
