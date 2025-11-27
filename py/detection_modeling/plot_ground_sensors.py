import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm

# Optical-only subset with 5σ limiting mag, nightly coverage, cadence info
optical_surveys = [
    {
        "Survey": "Pan-STARRS (PS1/PS2)",
        "LimitingMag_5sigma": 22.0,
        "Coverage_deg2_per_night": 1000,
        "Cadence_days": 1,
        "Fixed_cost": 100000000,
        "Rec_cost": 10000000,
    },
    {
        "Survey": "Catalina Sky Survey (CSS 0.7m)",
        "LimitingMag_5sigma": 19.5,
        "Coverage_deg2_per_night": 4000,
        "Cadence_days": 1,
        "Fixed_cost": 10000000,
        "Rec_cost": 3000000,
    },
    {
        "Survey": "ATLAS (per unit)",
        "LimitingMag_5sigma": 19.0,
        "Coverage_deg2_per_night": 6500,
        "Cadence_days": 1,
        "Fixed_cost": 5000000,
        "Rec_cost": 2000000,
    },
    {
        "Survey": "ZTF (Palomar 48\")",
        "LimitingMag_5sigma": 20.6,
        "Coverage_deg2_per_night": 9167,  # 27,500/3 nights
        "Cadence_days": 3,
        "Fixed_cost": 20000000,
        "Rec_cost": 5000000,
    },
    {
        "Survey": "Rubin/LSST (single visit)",
        "LimitingMag_5sigma": 24.5,
        "Coverage_deg2_per_night": 6000,  # ~18,000 deg² / 3 nights
        "Cadence_days": 3,
        "Fixed_cost": 700000000,
        "Rec_cost": 50000000,
    },
]

df_optical_specs = pd.DataFrame(optical_surveys)

# --- Parameters ---
p_v = 0.14
k = 0.7
D_range = np.logspace(1, 3, 50)    # 10–1000 m (log spaced)
R_range = np.linspace(0.05, 1.0, 50)  # 0.05–1.0 AU (avoid 0)
R_AU, D_m = np.meshgrid(R_range, D_range)

# --- Functions ---
def D_to_H(D_m):
    """Convert diameter (m) to absolute magnitude H."""
    D_km = D_m / 1000.0
    return 5 * np.log10(1329 / (D_km * np.sqrt(p_v)))

def m_app(H, R_AU):
    """Apparent magnitude assuming r=1 AU, phase ~ 0."""
    return H + 5 * np.log10(R_AU)

def P_det(m, m_lim, k=0.5):
    """Logistic detection probability."""
    return 1 / (1 + np.exp((m - m_lim) / k))

# --- Plot ---
fig, axes = plt.subplots(2, 3, figsize=(16, 8), constrained_layout=True)
axes = axes.flatten()

for i, row in df_optical_specs.iterrows():
    m_lim = row["LimitingMag_5sigma"]
    survey_name = row["Survey"]

    H_val = D_to_H(D_m)
    m = m_app(H_val, R_AU)
    P = P_det(m, m_lim, k)  # <-- single-exposure probability (no coverage)

    im = axes[i].pcolormesh(
        R_AU, D_m, P, shading="auto", cmap=cm.viridis, vmin=0, vmax=1
    )
    axes[i].set_title(survey_name, fontsize=11)
    axes[i].set_xlabel("Range Δ (AU)")
    axes[i].set_ylabel("Size (m)")
    axes[i].set_yscale("log")
    fig.colorbar(im, ax=axes[i], label="Detection Probability")

# Remove unused panels
for j in range(len(df_optical_specs), len(axes)):
    fig.delaxes(axes[j])

# Add a large (suptitle) for the whole figure
fig.suptitle("Single Exposure Detection Probability", fontsize=18, fontweight="bold")


fig2, axes2 = plt.subplots(2, 3, figsize=(16, 8), constrained_layout=True)
axes2 = axes2.flatten()

TOTAL_SKY_DEG2 = 41253  # full-sky area

for i, row in df_optical_specs.iterrows():
    m_lim = row["LimitingMag_5sigma"]
    survey_name = row["Survey"]
    f_sky = row["Coverage_deg2_per_night"] / TOTAL_SKY_DEG2

    H_val = D_to_H(D_m)
    m = m_app(H_val, R_AU)
    P_single = P_det(m, m_lim, k)
    P_24h = f_sky * P_single  # coverage-adjusted probability

    im = axes2[i].pcolormesh(
        R_AU, D_m, P_single, shading="auto", cmap=cm.inferno, vmin=0, vmax=1
    )
    axes2[i].set_title(f"{survey_name}\n(f_sky = {f_sky:.3f})", fontsize=11)
    axes2[i].set_xlabel("Range Δ (AU)")
    axes2[i].set_ylabel("Size (m)")
    axes2[i].set_yscale("log")
    fig2.colorbar(im, ax=axes2[i], label="Detection Probability in 24 h")

for j in range(len(df_optical_specs), len(axes2)):
    fig2.delaxes(axes2[j])

fig2.suptitle("Probability of Detection in 24 h (with Sky Coverage)", fontsize=18, fontweight="bold")

plt.savefig("plots/ground_sensors.png")
