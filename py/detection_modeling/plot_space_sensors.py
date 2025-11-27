import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm

# --------------------------
# Constants / Unit helpers
# --------------------------
AU_M = 1.495978707e11        # meters
SIGMA = 5.670374419e-8       # W m^-2 K^-4 (Stefan-Boltzmann)
h = 6.62607015e-34           # J s (Planck)
c = 2.99792458e8             # m s^-1 (speed of light)
kB = 1.380649e-23            # J K^-1 (Boltzmann)
JY_W_M2_HZ = 1e-26           # 1 Jy in W m^-2 Hz^-1
MJY_W_M2_HZ = 1e-29          # 1 mJy in W m^-2 Hz^-1

# --------------------------
# Survey table (IR, flux-density limits in mJy)
# You can add more missions/bands here.
# 'Fnu_lim_mJy' is the 5σ flux-density limit per exposure in mJy.
# 'lambda_um' is the band center (approx; narrow-band approximation).
# --------------------------
ir_surveys = [
    {
        "Survey": "NEOWISE (W2 4.6 μm)",
        "Fnu_lim_mJy": 0.11,       # ~0.11 mJy 5σ (point source)
        "lambda_um": 4.6,
        "Coverage_deg2_per_day": 230,  # crude daily average over full-sky in 6 months
        "Cadence_days": 1,
    },
    {
        "Survey": "AKARI (S9W 9 μm)",
        "Fnu_lim_mJy": 50.0,       # ~50 mJy per scan (order-of-magnitude)
        "lambda_um": 9.0,
        "Coverage_deg2_per_day": 230,  # all-sky scan style (placeholder)
        "Cadence_days": 1,
    },
    # Example placeholder for NEO Surveyor (set when you have finalized limits)
    {
        "Survey": "NEO Surveyor (NC2 6–10 μm, ref @ 8 μm)",
        "Fnu_lim_mJy": 0.5,        # placeholder sub-mJy sensitivity
        "lambda_um": 8.0,
        "Coverage_deg2_per_day": 3000, # placeholder wide-area scan/day
        "Cadence_days": 1,
    },
]

df_ir_specs = pd.DataFrame(ir_surveys)

# --------------------------
# Model parameters
# --------------------------
A_BOND = 0.10    # Bond albedo (dimensionless) — typical for NEOs
EPS = 0.90       # Emissivity (dimensionless)
ETA = 1.0        # Beaming factor (set to 1 for simple equilibrium)
k_fluxdex = 0.25 # logistic roll-off width in log10(flux) (dex)

# Size–range grids
D_range = np.logspace(1, 3, 50)     # 10–1000 m (log spaced)
Delta_range = np.linspace(0.05, 1.0, 50)  # geocentric distance Δ: 0.05–1.0 AU
Delta_AU, D_m = np.meshgrid(Delta_range, D_range)

# For temperature we also need heliocentric distance r_au. Use 1 AU baseline here.
r_au = 1.0

# --------------------------
# Physics helpers (pay attention to units!)
# --------------------------
def T_equilibrium(r_au, A=A_BOND, eps=EPS, eta=ETA):
    """
    Equilibrium (subsolar/NEATM-like simple) temperature in Kelvin.
    Using a commonly-used scaling: ~278 K at 1 AU for A~0, then adjust.
    We include (1-A)^(1/4) / (eps*eta)^(1/4) and r_au^(-1/2).
    """
    return 278.0 * ((1 - A) / (eps * eta))**0.25 * r_au**(-0.5)

def planck_lambda_W_m2_sr_m(lam_m, T):
    """
    Planck spectral radiance B_λ(T) in W m^-2 sr^-1 m^-1 (per meter).
    """
    x = (h * c) / (lam_m * kB * T)
    return (2.0 * h * c**2) / (lam_m**5) / (np.expm1(x))

def Fnu_asteroid_mJy(D_m, Delta_AU, lam_um, T, eps=EPS):
    """
    Monochromatic flux density (approx) at band center (narrow-band assumption).
    Returns F_nu in mJy.

    Steps:
      1) Compute B_lambda(T) [W m^-2 sr^-1 m^-1]
      2) Convert to exitance via π: M_lambda = ε π B_lambda [W m^-2 m^-1]
      3) Total power per wavelength: P_lambda = M_lambda * 4πR^2
      4) Flux per wavelength at distance Δ: F_lambda = P_lambda / (4π Δ^2)
         -> simplifies to: ε π B_lambda * R^2 / Δ^2
      5) Convert F_lambda (per meter) to F_nu (per Hz) via F_nu = (λ^2 / c) * F_lambda
      6) Convert to mJy.
    """
    lam_m = lam_um * 1e-6                 # μm -> m
    R_m = 0.5 * D_m                       # radius
    Delta_m = Delta_AU * AU_M             # AU -> m

    B_lam = planck_lambda_W_m2_sr_m(lam_m, T)                # W m^-2 sr^-1 m^-1
    F_lam = eps * np.pi * B_lam * (R_m**2) / (Delta_m**2)    # W m^-2 m^-1
    F_nu_W_m2_Hz = (lam_m**2 / c) * F_lam                    # W m^-2 Hz^-1
    F_nu_mJy = F_nu_W_m2_Hz / MJY_W_M2_HZ                    # to mJy
    return F_nu_mJy

def P_det_IR(F_nu_mJy, Fnu_lim_mJy, kdex=k_fluxdex):
    """
    Logistic detection probability in log10(flux) space.
    Higher flux than the limit -> probability → 1.
    """
    logF = np.log10(np.maximum(F_nu_mJy, 1e-40))
    logFlim = np.log10(Fnu_lim_mJy)
    return 1.0 / (1.0 + np.exp((logFlim - logF) / kdex))

# --------------------------
# Plot 1: Single Exposure (IR, flux-based)
# --------------------------
fig, axes = plt.subplots(2, 3, figsize=(16, 8), constrained_layout=True)
axes = axes.flatten()

T_K = T_equilibrium(r_au)  # K (scalar here; could be a grid if you vary r_au)

for i, row in df_ir_specs.iterrows():
    survey = row["Survey"]
    lam_um = row["lambda_um"]
    Fnu_lim = row["Fnu_lim_mJy"]

    Fnu = Fnu_asteroid_mJy(D_m, Delta_AU, lam_um, T_K)    # mJy
    P = P_det_IR(Fnu, Fnu_lim, k_fluxdex)

    im = axes[i].pcolormesh(
        Delta_AU, D_m, P, shading="auto", cmap=cm.viridis, vmin=0, vmax=1
    )
    axes[i].set_title(f"{survey}\nλ = {lam_um} μm, F_lim ≈ {Fnu_lim:g} mJy", fontsize=11)
    axes[i].set_xlabel("Geocentric Range Δ (AU)")
    axes[i].set_ylabel("Size D (m)")
    axes[i].set_yscale("log")
    cbar = fig.colorbar(im, ax=axes[i], label="Detection Probability (Single Exposure)")

# Remove unused panels
for j in range(len(df_ir_specs), len(axes)):
    fig.delaxes(axes[j])

fig.suptitle("Single Exposure Detection Probability (Space-based IR, Flux Limit)", fontsize=18, fontweight="bold")

# --------------------------
# Plot 2: 24 h probability with coverage
# f_sky = coverage_deg2_per_day / 41253
# P_24h = f_sky * P_single  (one look/chance in that day)
# --------------------------
fig2, axes2 = plt.subplots(2, 3, figsize=(16, 8), constrained_layout=True)
axes2 = axes2.flatten()

TOTAL_SKY_DEG2 = 41253.0

for i, row in df_ir_specs.iterrows():
    survey = row["Survey"]
    lam_um = row["lambda_um"]
    Fnu_lim = row["Fnu_lim_mJy"]
    f_sky = row["Coverage_deg2_per_day"] / TOTAL_SKY_DEG2

    Fnu = Fnu_asteroid_mJy(D_m, Delta_AU, lam_um, T_K)  # mJy
    P_single = P_det_IR(Fnu, Fnu_lim, k_fluxdex)
    P_24h = f_sky * P_single  # one effective chance/day; use 1-(1-f_sky*P)^N for N looks

    im = axes2[i].pcolormesh(
        Delta_AU, D_m, P_single, shading="auto", cmap=cm.inferno, vmin=0, vmax=1
    )
    axes2[i].set_title(f"{survey}\n(f_sky = {f_sky:.3f})", fontsize=11)
    axes2[i].set_xlabel("Geocentric Range Δ (AU)")
    axes2[i].set_ylabel("Size D (m)")
    axes2[i].set_yscale("log")
    cbar = fig2.colorbar(im, ax=axes2[i], label="Detection Probability in 24 h")

for j in range(len(df_ir_specs), len(axes2)):
    fig2.delaxes(axes2[j])

fig2.suptitle("Probability of Detection in 24 h (Space-based IR, with Coverage)", fontsize=18, fontweight="bold")

plt.savefig("plots/space_sensors.png")
