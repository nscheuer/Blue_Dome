import numpy as np
import matplotlib.pyplot as plt

class SizeDistribution:
    """Power Law Distribution"""
    def __init__(self, b: float, Dmin: float = 0.001, Dmax: float = 100.0):
        self.b = b
        self.Dmin = Dmin
        self.Dmax = Dmax

        # Precompute constants for efficiency
        self.A = Dmin**(-b)
        self.B = Dmax**(-b)
        self.norm = self.A - self.B  # normalizing denominator

    # ----------------------------------------------------
    def pdf(self, D: float) -> float:
        """Probability density function p(D). Automatically normalized."""
        if D < self.Dmin or D > self.Dmax:
            return 0.0
        C = self.b / self.norm
        return C * D**(-(self.b + 1))

    # ----------------------------------------------------
    def cdf(self, D: float) -> float:
        """Cumulative distribution function F(D)."""
        if D <= self.Dmin:
            return 0.0
        if D >= self.Dmax:
            return 1.0
        return (self.A - D**(-self.b)) / self.norm

    # ----------------------------------------------------
    def probability_between(self, D1: float, D2: float) -> float:
        """P(D1 < D < D2)."""
        D1, D2 = min(D1, D2), max(D1, D2)
        return self.cdf(D2) - self.cdf(D1)

    # ----------------------------------------------------
    def sample(self, n: int = 1) -> np.ndarray:
        """Generate random samples from the analytic SFD."""
        u = np.random.rand(n)
        inner = self.A - u * (self.A - self.B)
        return inner**(-1.0 / self.b)

    # ----------------------------------------------------
    def plot_pdf(self, npts: int = 500):
        """Plot the analytic PDF p(D)."""
        Dvals = np.logspace(np.log10(self.Dmin), np.log10(self.Dmax), npts)
        pdf_vals = [self.pdf(D) for D in Dvals]

        plt.figure(figsize=(6, 4))
        plt.loglog(Dvals, pdf_vals, label=f"PDF (b={self.b})")
        plt.xlabel("Diameter D (km)")
        plt.ylabel("PDF p(D)")
        plt.grid(True, which="both", ls="--", alpha=0.4)
        plt.legend()
        plt.tight_layout()

    # ----------------------------------------------------
    def plot_cdf(self, npts: int = 500):
        """Plot the analytic CDF F(D)."""
        Dvals = np.logspace(np.log10(self.Dmin), np.log10(self.Dmax), npts)
        cdf_vals = [self.cdf(D) for D in Dvals]

        plt.figure(figsize=(6, 4))
        plt.semilogx(Dvals, cdf_vals, label=f"CDF (b={self.b})")
        plt.xlabel("Diameter D (km)")
        plt.ylabel("CDF F(D)")
        plt.grid(True, which="both", ls="--", alpha=0.4)
        plt.legend()
        plt.tight_layout()
