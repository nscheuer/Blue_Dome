import numpy as np
import matplotlib.pyplot as plt

from sensors.combined_sensor import CombinedSensor
from neo_models.neo_distribution import NEODistribution
from neo_models.size_distribution import SizeDistribution

AU_M = 1.495978707e11  # m
DAY_S = 86400.0        # s/day


class HazardFunction:
    def __init__(
        self,
        combined_sensor: CombinedSensor,
        size_dist: SizeDistribution,
        vel_dist: NEODistribution,
        pv_dist: NEODistribution,
        tau_max_days: float = 365.0 * 50.0,
        dt_days: float = 1.0,
        n_mc: int = 10_000
    ) -> None:

        self.combined_sensor = combined_sensor
        self.size_dist = size_dist
        self.vel_dist = vel_dist
        self.pv_dist = pv_dist
        self.tau_max_days = tau_max_days
        self.dt_days = dt_days
        self.n_mc = n_mc

        self.tau_grid = None
        self.pdf_tti = None
        self.cdf_tti = None
        self.detection_fraction = None
        self.survival = None  # <-- new

        # Auto compute
        self.compute_tti_distribution()

    # --------------------------------------------------------
    def _expected_P_det_24h(self, tau_days: float) -> float:
        D_m = self.size_dist.sample(n=self.n_mc)
        v_ms = self.vel_dist.sample(n=self.n_mc)
        p_v = self.pv_dist.sample(n=self.n_mc)

        tau_s = tau_days * DAY_S
        r_m = v_ms * tau_s
        Geocentric_AU = r_m / AU_M

        P_det = self.combined_sensor.F_tot(D_m, Geocentric_AU, p_v)
        return float(np.mean(P_det))

    # --------------------------------------------------------
    def compute_tti_distribution(self):
        # 1. Setup Grid (0 to Max)
        n_steps = int(np.ceil(self.tau_max_days / self.dt_days))
        tau_edges = np.linspace(0.0, self.tau_max_days, n_steps + 1)
        tau_grid = 0.5 * (tau_edges[:-1] + tau_edges[1:])

        # 2. Pre-compute instantaneous detection probability for all time steps
        #    This is purely geometric/sensor based, independent of history.
        #    (We vectorize or loop this. Keeping your loop style for clarity,
        #    but filling an array first).
        raw_probs = np.zeros(n_steps)
        for k in range(n_steps):
            raw_probs[k] = self._expected_P_det_24h(tau_grid[k])

        # 3. Integrate Survival BACKWARDS (from Max TTI down to 0)
        #    S represents: Probability object is NOT YET detected.
        #    At tau_max, S = 1.0 (we haven't started looking yet).

        pdf_tti = np.zeros(n_steps)
        S_curr = 1.0

        # We assume tau_grid is ordered [0, 1, ... max].
        # We iterate in reverse: indices n-1 down to 0.
        for k in reversed(range(n_steps)):
            lambda_k = raw_probs[k]

            # Probability of FIRST detection at this step:
            # P(detect now) * P(survived until now)
            pdf_tti[k] = lambda_k * S_curr

            # Update survival for the next step (which is closer to Earth)
            S_curr = S_curr * (1.0 - lambda_k)

        # 4. Post-process
        # The total fraction of objects detected is sum(pdf)
        # or 1.0 - S_final (where S_final is survival at impact)
        detection_fraction = np.sum(pdf_tti)

        # S_curr is now the survival probability at tau=0 (Impact).
        # To plot the full survival curve S(tau), we need to reconstruct the array
        # aligned with tau_grid.

        # Let's rebuild the S array for plotting purposes
        survival_curve = np.zeros(n_steps)
        s_temp = 1.0
        for k in reversed(range(n_steps)):
            survival_curve[k] = s_temp
            s_temp *= (1.0 - raw_probs[k])

        # Normalize PDF if needed (standard practice is usually to leave it
        # scaled by completeness so area under curve = efficiency,
        # but your code normalizes it to 1.0 for the shape).
        if detection_fraction > 0:
            pdf_normalized = pdf_tti / detection_fraction
            cdf_tti = np.cumsum(pdf_normalized) # This accumulates 0 -> Max
        else:
            pdf_normalized = pdf_tti
            cdf_tti = np.zeros_like(pdf_tti)

        # Store
        self.tau_grid = tau_grid
        self.pdf_tti = pdf_normalized # Shape of detection times
        self.cdf_tti = cdf_tti
        self.detection_fraction = detection_fraction
        self.survival = survival_curve

        return tau_grid, pdf_normalized, cdf_tti, detection_fraction


    def time_to_detect_percent(self, target: float = 0.03) -> float:
        """
        Calculate the TTI at which the survival probability exceeds a target threshold.

        This finds the "crossing point" where the probability of *not* detecting
        the object rises above `target`.

        - If survival is always > target (sensor is too weak), returns 0.0.
        - If survival is always <= target (sensor is perfect), returns tau_max.
        - Otherwise, interpolates the exact time the curve crosses the target.

        Parameters
        ----------
        target : float
            The allowable survival fraction (e.g., 0.03 for 97% completeness).

        Returns
        -------
        float
            The TTI (days) where the survival curve crosses the target.
        """
        if self.survival is None or self.tau_grid is None:
            raise RuntimeError("compute_tti_distribution() has not run.")

        # 1. Edge Case: Sensor is so poor it never gets risk below target
        #    (Even at tau=0, the survival/risk is > target)
        if self.survival[0] > target:
            return 0.0

        # 2. Edge Case: Sensor is so good risk is always below target
        #    (Even at tau_max, the survival/risk is <= target)
        if self.survival[-1] <= target:
            return self.tau_max_days

        # 3. Standard Case: The curve crosses the target.
        #    Find the first index where survival > target.
        #    Since survival increases with tau (0 -> 1), we look for the first 'True'.
        idx = np.argmax(self.survival > target)

        # 4. Linear Interpolation
        #    We know the crossing happened between idx-1 and idx
        t_low = self.tau_grid[idx - 1]
        s_low = self.survival[idx - 1]

        t_high = self.tau_grid[idx]
        s_high = self.survival[idx]

        # Solve for t where S(t) == target
        # slope = (s_high - s_low) / (t_high - t_low)
        # target = s_low + slope * (t_cross - t_low)
        # t_cross = t_low + (target - s_low) / slope

        fraction = (target - s_low) / (s_high - s_low)
        t_crossing = t_low + fraction * (t_high - t_low)

        return float(t_crossing)

    # --------------------------------------------------------
    # PLOTTING FUNCTIONS
    # --------------------------------------------------------

    def plot_tti_pdf(self):
        """Plot PDF of Time-To-Impact at first detection."""
        if self.pdf_tti is None:
            raise RuntimeError("compute_tti_distribution() has not run.")

        plt.figure(figsize=(7, 4))
        plt.plot(self.tau_grid, self.pdf_tti, lw=2)
        plt.xlabel("Time-To-Impact τ (days)")
        plt.ylabel("PDF of first detection")
        plt.title("TTI Distribution (PDF)")
        plt.grid(True, alpha=0.3)
        plt.tight_layout()


    def plot_tti_cdf(self):
        """Plot CDF of Time-To-Impact at first detection."""
        if self.cdf_tti is None:
            raise RuntimeError("compute_tti_distribution() has not run.")

        plt.figure(figsize=(7, 4))
        plt.plot(self.tau_grid, self.cdf_tti, lw=2)
        plt.xlabel("Time-To-Impact τ (days)")
        plt.ylabel("CDF of first detection")
        plt.title("TTI Distribution (CDF)")
        plt.grid(True, alpha=0.3)
        plt.tight_layout()


    def plot_survival(self):
        """Plot survival S(τ): probability object has NOT yet been detected."""
        if self.survival is None:
            raise RuntimeError("compute_tti_distribution() has not run.")

        plt.figure(figsize=(7, 4))
        plt.plot(self.tau_grid, self.survival, lw=2)
        plt.xlabel("Time-To-Impact τ (days)")
        plt.ylabel("Survival S(τ)")
        plt.title("Survival Curve (Probability object has NOT yet been detected when TTI=tau)")
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
