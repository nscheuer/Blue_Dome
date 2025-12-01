import numpy as np
import matplotlib.pyplot as plt

class NEODistribution:
    def __init__(self, name: str, property: np.ndarray, fraction: np.ndarray) -> None:
        self.name = name
        self.x = property
        self.y = fraction

        area = np.trapezoid(self.y, self.x)
        if area == 0:
            raise ValueError("Distribution has zero area - check data")
        self.pdf = self.y/area

        self.cdf = np.cumsum(self.pdf)
        self.cdf /= self.cdf[-1]

    def pdf_at(self, x: float) -> float:
        return float(np.interp(x, self.x, self.pdf))
    
    def cdf_at(self, x: float) -> float:
        return float(np.interp(x, self.x, self.cdf))
    
    def sample(self, n: int = 1) -> np.ndarray:
        """Generate n random samples from the distribution."""
        u = np.random.uniform(0, 1, size=n)
        samples = np.interp(u, self.cdf, self.x)
        return samples
    
    def plot_pdf(self):
        plt.figure(figsize=(6, 4))
        plt.plot(self.x, self.pdf, label=f"{self.name} PDF")
        plt.xlabel(self.name)
        plt.ylabel("PDF")
        plt.grid(True, alpha=0.3)
        plt.legend()
        plt.tight_layout()
        plt.show()

    def plot_cdf(self):
        plt.figure(figsize=(6, 4))
        plt.plot(self.x, self.cdf, label=f"{self.name} CDF")
        plt.xlabel(self.name)
        plt.ylabel("CDF")
        plt.grid(True, alpha=0.3)
        plt.legend()
        plt.tight_layout()
        plt.show()