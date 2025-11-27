module Plot where

import Types

import Granite (defPlot, heatmap, plotTitle)

prepare :: Heatmap -> Matrix Double
prepare = map (map snd)

plot :: Heatmap -> IO ()
plot matrix =
  putTextLn $
    heatmap
      (prepare matrix)
      defPlot{plotTitle = toText "Sensor detection probability heatmap"}
