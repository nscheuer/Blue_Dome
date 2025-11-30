-- Sensor data and other statistics
module Data where

import Types

starrs :: SensorData
starrs =
  SensorData
    { survey = "Pan-STARRS (PS1/PS2)"
    , sensorName = "starrs"
    , limit = 22.0
    , coverage = SquareDegree 1000
    , fixedCost = 1e8
    , recurringCost = 1e7
    }

catalina :: SensorData
catalina =
  SensorData
    { survey = "Catalina Sky Survey (CSS 0.7m)"
    , sensorName = "catalina"
    , limit = 19.5
    , coverage = SquareDegree 1000
    , fixedCost = 1e7
    , recurringCost = 3e6
    }

atlas :: SensorData
atlas =
  SensorData
    { survey = "ATLAS (per unit)"
    , sensorName = "atlas"
    , limit = 19.0
    , coverage = SquareDegree 6500
    , fixedCost = 5e6
    , recurringCost = 2e6
    }

ztf :: SensorData
ztf =
  SensorData
    { survey = "ZTF (Palomar 48)"
    , sensorName = "ztf"
    , limit = 20.6
    , coverage = SquareDegree 9167
    , fixedCost = 2e7
    , recurringCost = 5e6
    }

rubin :: SensorData
rubin =
  SensorData
    { survey = "Rubin/LSST (main lens)"
    , sensorName = "rubin"
    , limit = 24.5
    , coverage = SquareDegree 6000
    , fixedCost = 7e8
    , recurringCost = 5e7
    }

sensors :: [SensorData]
sensors = [starrs, catalina, atlas, ztf, rubin]
