from sensors.space_sensor import SpaceSensor

NEOWISE = SpaceSensor(name="NEOWISE", Fnu_lim_mJy=0.11, lambda_um=4.6, deg2perday=230.0, fixedcost=230_000_000, reccost=5_000_000)
AKARI = SpaceSensor(name="AKARI", Fnu_lim_mJy=50.0, lambda_um=9.0, deg2perday=230.0, fixedcost=120_000_000, reccost=5_000_000)
IRAS = SpaceSensor(name="IRAS", Fnu_lim_mJy=500.0, lambda_um=12.0, deg2perday=230.0, fixedcost=200_000_000, reccost=5_000_000)
WISE = SpaceSensor(name="WISE", Fnu_lim_mJy=6.0, lambda_um=22.0, deg2perday=230.0, fixedcost=320_000_000, reccost=8_000_000)
NEO_SURVEYOR = SpaceSensor(name="NEO Surveyor", Fnu_lim_mJy=0.5, lambda_um=8.0, deg2perday=3000.0, fixedcost=1_200_000_000, reccost=50_000_000)
NEOMIR = SpaceSensor(name="NEOMIR", Fnu_lim_mJy=1.0, lambda_um=8.0, deg2perday=3000.0, fixedcost=550_000_000, reccost=25_000_000)
SENTINEL = SpaceSensor(name="Sentinel", Fnu_lim_mJy=1.0, lambda_um=10.0, deg2perday=3000.0, fixedcost=450_000_000, reccost=30_000_000)
NEOCAM = SpaceSensor(name="NEOCam", Fnu_lim_mJy=0.5, lambda_um=8.0, deg2perday=3000.0, fixedcost=600_000_000, reccost=40_000_000)
SPITZER = SpaceSensor(name="Spitzer-IRAC", Fnu_lim_mJy=0.01, lambda_um=4.5, deg2perday=20.0, fixedcost=800_000_000, reccost=30_000_000)
GAIA = SpaceSensor(name="Gaia", Fnu_lim_mJy=0.02, lambda_um=0.64, deg2perday=650.0, fixedcost=1_000_000_000, reccost=40_000_000)

SPITZER.plot_detection_maps()
