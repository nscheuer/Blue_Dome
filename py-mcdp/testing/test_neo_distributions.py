from neo_models.neo_distribution import NEODistribution
from neo_models.size_distribution import SizeDistribution
import numpy as np

# Data taken from: The population of near-earth asteroids revisited and updated, Alan W. Harris, Paul W. Chodas

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

Size_Distribution = SizeDistribution(b=1.35)

Size_Distribution.plot_pdf()
