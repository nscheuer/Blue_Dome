from sensors.ground_sensor import GroundSensor
from sensors.space_sensor import SpaceSensor
from sensors.close_all import create_close_all_button_window
from sensors.combined_sensor import CombinedSensor

RUBIN     = GroundSensor(name="Rubin/LSST (single visit)", m_fivesigma=24.5, deg2pernight=6000.0, fixedcost=700_000_000, reccost=50_000_000)

RUBIN.plot_detection_maps()

Combined = CombinedSensor(ground_sensors=[RUBIN]*10)
Combined.plot_detection_maps()
create_close_all_button_window()
