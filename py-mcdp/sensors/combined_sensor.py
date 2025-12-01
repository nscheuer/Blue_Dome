import numpy as np
import matplotlib.pyplot as plt
from typing import List
from sensors.ground_sensor import GroundSensor
from sensors.space_sensor import SpaceSensor


class CombinedSensor:
    """
    Combine multiple ground-based and space-based survey sensors
    into a single effective detection model.

    The combined detection probability assumes independent detections:

        F_tot = 1 - ∏_j (1 - F_j)

    This is the correct physics for parallel survey systems
    (multiple telescopes covering overlapping sky regions).
    """

    def __init__(
        self,
        ground_sensors: List[GroundSensor] = [],
        space_sensors: List[SpaceSensor] = []
    ):
        self.ground_sensors = ground_sensors
        self.space_sensors = space_sensors

    # --------------------------------------------------------------
    # Combined detection probability (24h)
    # --------------------------------------------------------------
    def F_tot(self, D_m: float, Geocentric_AU: float, p_v: float,
              Heliocentric_AU: float = 1.0,
              A_bond: float = 0.10, eps: float = 0.90, eta: float = 1.0,
              k_fluxdex: float = 0.5) -> float:
        """
        Combined detection probability over 24 hours for all sensors.
        """

        p_not = 1.0

        # Ground sensors (optical)
        for sensor in self.ground_sensors:
            p = sensor.detection_prob_24h(
                D_m=D_m, R_AU=Geocentric_AU, p_v=p_v
            )
            p_not *= (1.0 - p)

        # Space sensors (thermal-IR)
        for sensor in self.space_sensors:
            p = sensor.detection_prob_24h(
                D_m=D_m,
                Geocentric_AU=Geocentric_AU,
                Heliocentric_AU=Heliocentric_AU,
                A_bond=A_bond,
                eps=eps,
                eta=eta,
                k_fluxdex=k_fluxdex,
            )
            p_not *= (1.0 - p)

        return 1.0 - p_not

    # --------------------------------------------------------------
    # Plot detection maps for the entire combined system
    # --------------------------------------------------------------
    def plot_detection_maps(
        self,
        D_range=None,
        Geocentric_range_AU=None,
        p_v: float = 0.14,
        Heliocentric_AU: float = 1.0,
        A_bond: float = 0.10,
        eps: float = 0.90,
        eta: float = 1.0,
        k_fluxdex: float = 0.5,
        cmap_single: str = "viridis",
        cmap_24h: str = "inferno",
    ):
        """
        Plot (1) combined single-exposure probability and (2) 24h detection
        probability for all sensors acting together.

        Since ground and space sensors define single-exposure probability
        differently, we approximate the combined single-exposure detection
        as:

            F_single_tot = 1 - ∏_j (1 - F_single_j)

        and compute 24h detection inside F_tot().
        """

        # Use default grids
        if D_range is None:
            D_range = np.logspace(1, 3, 50)  # 10–1000 m
        if Geocentric_range_AU is None:
            Geocentric_range_AU = np.linspace(0.05, 1.0, 50)

        Δ_grid, D_grid = np.meshgrid(Geocentric_range_AU, D_range)

        # ----------------------------------------------------------
        # Compute single-exposure combined probability
        # ----------------------------------------------------------
        P_single_tot = np.ones_like(D_grid)

        # Ground sensors: single-exposure comes from magnitude model
        for sensor in self.ground_sensors:
            H = sensor._absolute_magnitude(D_grid, p_v)
            m = sensor._apparent_magntitude(H, Δ_grid)
            P_single = sensor._single_exposure_prob(
                m, sensor.m_fivesigma
            )
            P_single_tot *= (1.0 - P_single)

        # Space sensors: single-exposure comes from flux model
        for sensor in self.space_sensors:
            T = sensor._equilibrium_temp(
                Heliocentric_AU, A_bond, eps, eta
            )
            Fnu = sensor._flux_mJy(D_grid, Δ_grid, T, eps)
            P_single = sensor._single_exposure_prob(
                Fnu, sensor.Fnu_lim_mJy, k=k_fluxdex
            )
            P_single_tot *= (1.0 - P_single)

        P_single_tot = 1.0 - P_single_tot

        # ----------------------------------------------------------
        # Compute 24h combined probability (full physics)
        # ----------------------------------------------------------
        P_24h = np.zeros_like(D_grid)
        for i in range(D_grid.shape[0]):
            for j in range(D_grid.shape[1]):
                P_24h[i, j] = self.F_tot(
                    D_m=D_grid[i, j],
                    Geocentric_AU=Δ_grid[i, j],
                    Heliocentric_AU=Heliocentric_AU,
                    p_v=p_v,
                    A_bond=A_bond,
                    eps=eps,
                    eta=eta,
                    k_fluxdex=k_fluxdex,
                )

        # ----------------------------------------------------------
        # Panel 1: Single-exposure probability
        # ----------------------------------------------------------
        fig, ax = plt.subplots(figsize=(7, 5))
        im1 = ax.pcolormesh(
            Δ_grid, D_grid, P_single_tot,
            shading="auto", cmap=cmap_single, vmin=0, vmax=1
        )
        ax.set_title("Combined Single-Exposure Detection Probability")
        ax.set_xlabel("Geocentric Distance Δ (AU)")
        ax.set_ylabel("Diameter D (m)")
        ax.set_yscale("log")
        fig.colorbar(im1, ax=ax)

        # ----------------------------------------------------------
        # Panel 2: 24h combined probability
        # ----------------------------------------------------------
        fig2, ax2 = plt.subplots(figsize=(7, 5))
        im2 = ax2.pcolormesh(
            Δ_grid, D_grid, P_24h,
            shading="auto", cmap=cmap_24h, vmin=0, vmax=1
        )
        ax2.set_title("Combined 24 h Detection Probability")
        ax2.set_xlabel("Geocentric Distance Δ (AU)")
        ax2.set_ylabel("Diameter D (m)")
        ax2.set_yscale("log")
        fig2.colorbar(im2, ax=ax2)
