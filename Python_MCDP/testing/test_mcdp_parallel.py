import numpy as np
import matplotlib.pyplot as plt
import multiprocessing

# Import your custom modules
from sensors.ground_sensor import GroundSensor
from sensors.space_sensor import SpaceSensor
from sensors.close_all import create_close_all_button_window
from neo_models.neo_distribution import NEODistribution
from neo_models.size_distribution import SizeDistribution
from sensors.combined_sensor import CombinedSensor
from hazard.hazard import HazardFunction
from mcdp.MCDP_parallel import MCDP_Parallel

# ==========================================
# 1. Define Sensors and Distributions (Global Scope)
# ==========================================
# It is safe to define these at the top level. When worker processes spawn,
# they will read these definitions, which is necessary so they can reconstruct the objects.

# --- Ground Sensors ---
PANSTARRS = GroundSensor(name="Pan-STARRS (PS1/PS2)", m_fivesigma=22.0, deg2pernight=1000.0, fixedcost=100_000_000, reccost=10_000_000)
CSS_07M   = GroundSensor(name="Catalina Sky Survey (CSS 0.7m)", m_fivesigma=19.5, deg2pernight=4000.0, fixedcost=10_000_000, reccost=3_000_000)
ATLAS     = GroundSensor(name="ATLAS (per unit)", m_fivesigma=19.0, deg2pernight=6500.0, fixedcost=5_000_000, reccost=2_000_000)
ZTF       = GroundSensor(name="ZTF (Palomar 48\")", m_fivesigma=20.6, deg2pernight=9167.0, fixedcost=20_000_000, reccost=5_000_000)
RUBIN     = GroundSensor(name="Rubin/LSST (single visit)", m_fivesigma=24.5, deg2pernight=6000.0, fixedcost=700_000_000, reccost=50_000_000)

# --- Space Sensors ---
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

# --- Distributions ---
Velocity_Distribution = NEODistribution(
    name="Velocity Distribution",
    property=np.array([0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60]),
    fraction=np.array([0, 0.005, 0.03, 0.06, 0.08, 0.07, 0.06, 0.035, 0.02, 0.01, 0.0, 0.0, 0.0])
    )

Albedo_Distribution = NEODistribution(
    name="Albedo Distribution",
    property=np.array([0.01, 0.02, 0.03, 0.05, 0.08, 0.1, 0.2, 0.3, 0.5, 0.8, 1]),
    fraction=np.array([0.05, 0.06, 0.18, 0.07, 0.12, 0.12, 0.23, 0.1, 0.03, 0.0, 0.0])
)

Size_Distribution = SizeDistribution(b=1.35, Dmin=0.02)


# ==========================================
# 2. Main Execution Function
# ==========================================
def main():
    """
    This function holds the logic that should ONLY run in the main process.
    """
    mcdp = MCDP_Parallel(
        ground_sensors=[RUBIN, ZTF, PANSTARRS], 
        space_sensors=[NEO_SURVEYOR, NEOCAM, NEOMIR, GAIA], 
        ground_max_counts=[3, 3, 3], 
        space_max_counts=[2, 2, 2, 2], 
        size_dist=Size_Distribution, 
        vel_dist=Velocity_Distribution, 
        pv_dist=Albedo_Distribution
    )
    
    # Run the simulation
    mcdp.run()
    
    # Generate the plot
    mcdp.plot_pareto()
    
    # Show the GUI elements
    create_close_all_button_window()
    plt.show()

# ==========================================
# 3. Entry Point Guard (Crucial for Windows)
# ==========================================
if __name__ == "__main__":
    # This prevents the infinite loop of spawning processes
    multiprocessing.freeze_support()
    main()