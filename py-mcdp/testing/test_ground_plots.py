from sensors.ground_sensor import GroundSensor

PANSTARRS = GroundSensor(name="Pan-STARRS (PS1/PS2)", m_fivesigma=22.0, deg2pernight=1000.0, fixedcost=100_000_000, reccost=10_000_000)
CSS_07M   = GroundSensor(name="Catalina Sky Survey (CSS 0.7m)", m_fivesigma=19.5, deg2pernight=4000.0, fixedcost=10_000_000, reccost=3_000_000)
ATLAS     = GroundSensor(name="ATLAS (per unit)", m_fivesigma=19.0, deg2pernight=6500.0, fixedcost=5_000_000, reccost=2_000_000)
ZTF       = GroundSensor(name="ZTF (Palomar 48\")", m_fivesigma=20.6, deg2pernight=9167.0, fixedcost=20_000_000, reccost=5_000_000)
RUBIN     = GroundSensor(name="Rubin/LSST (single visit)", m_fivesigma=24.5, deg2pernight=6000.0, fixedcost=700_000_000, reccost=50_000_000)

RUBIN.plot_detection_maps()
