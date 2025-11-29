import numpy as np
import matplotlib.pyplot as plt

# Physical Constants
AU_M = 1.495978707e11       # meters in one astronomical unit
h = 6.62607015e-34          # Planck constant [J·s]
c = 2.99792458e8            # speed of light [m/s]
kB = 1.380649e-23           # Boltzmann constant [J/K]
MJY_W_M2_HZ = 1e-29         # 1 mJy = 1e-29 W m^-2 Hz^-1


class SpaceSensor:
    r"""
    Space-based thermal-infrared asteroid survey model.

    This class implements a minimal version of standard asteroid
    thermal-IR detection physics following:

    - **Planck (1901)**, *On the Law of Distribution of Energy in the Normal Spectrum*  
    - **Harris & Lagerros (2002)**, *Asteroids in the Thermal Infrared* (NEATM)  
    - **Lebofsky & Spencer (1989)**, in *Asteroids II*, thermal equilibrium  
    - **Wright et al. (2010)**, *WISE/NEOWISE: Design & Flux Conversion*  
    - **Mainzer et al. (2011)**, *NEOWISE NEO Detection Pipeline*  

    Detection probability uses an ad-hoc logistic roll-off in log flux,
    a common practice in survey sensitivity modeling.
    """

    def __init__(self, name, Fnu_lim_mJy, lambda_um, deg2perday,
                 fixedcost=0.0, reccost=0.0):
        r"""
        Initialize a thermal-IR sensor.

        Parameters
        ----------
        name : str
            Name of the survey (e.g., ``"NEOWISE W2"``).
        Fnu_lim_mJy : float
            5σ flux-density limit in mJy.
            Based on WISE/NEOWISE conventions in
            **Wright et al. (2010)**, **Mainzer et al. (2011)**.
        lambda_um : float
            Band center wavelength in microns.
        deg2perday : float
            Daily survey sky coverage in deg².
        fixedcost, reccost : float
            Budget parameters (not used in physics).
        """
        self.name = name
        self.Fnu_lim_mJy = Fnu_lim_mJy
        self.lambda_um = lambda_um
        self.deg2perday = deg2perday
        self.fixedcost = fixedcost
        self.reccost = reccost

    def detection_prob_24h(self, D_m, Geocentric_AU, Heliocentric_AU,
                           A_bond, eps, eta, k_fluxdex=0.5):
        r"""
        Compute 24-hour detection probability.

        The physical sequence is:

        1. **Equilibrium temperature**  
           From NEATM and radiative balance  
           (Harris & Lagerros 2002; Lebofsky & Spencer 1989):

           .. math::
              T_{\rm eq} =
              278~{\rm K}\,
              \left(\frac{1-A_{\rm bond}}{\varepsilon \eta}\right)^{1/4}
              r_{\rm AU}^{-1/2}

        2. **Flux density**  
           Using Planck’s law (Planck 1901) and the WISE/NEOWISE
           flux-conversion recipe (Wright et al. 2010; Mainzer et al. 2011):

           .. math::
              F_\lambda = \varepsilon \pi B_\lambda
              \frac{R^2}{\Delta^2}

           .. math::
              F_\nu = \frac{\lambda^2}{c} F_\lambda

        3. **Detection probability per exposure**  
           Logistic model in log-flux, standard in survey simulations.

        4. **24 h probability**  
           .. math::
              P_{24} = f_{\rm sky} P_{\rm single},
              \qquad
              f_{\rm sky} = \frac{{\rm deg2perday}}{41253}.

        References
        ----------
        - Planck (1901)
        - Lebofsky & Spencer (1989)
        - Harris & Lagerros (2002)
        - Wright et al. (2010)
        - Mainzer et al. (2011)
        """
        T = self._equilibrium_temp(Heliocentric_AU, A_bond, eps, eta)
        Fnu = self._flux_mJy(D_m, Geocentric_AU, T, eps)
        P_single = self._single_exposure_prob(Fnu, self.Fnu_lim_mJy, k=k_fluxdex)
        f_sky = self.deg2perday / 41253.0
        return P_single * f_sky

    def _equilibrium_temp(self, Heliocentric_AU, A_bond, eps, eta):
        r"""
        Radiative-equilibrium temperature.

        Derived from instantaneous energy balance and the NEATM model:

        (Lebofsky & Spencer 1989; Harris & Lagerros 2002)

        .. math::
           T_{\rm eq} \propto
           \left(\frac{1-A_{\rm bond}}{\varepsilon \eta}\right)^{1/4}
           r^{-1/2}.

        Normalized to 278 K at 1 AU for a zero-albedo blackbody,
        matching standard solar-constant scalings.
        """
        return 278.0 * ((1 - A_bond) / (eps * eta)) ** 0.25 * Heliocentric_AU ** (-0.5)

    def _planck_spectral_radiance(self, lam_m, T):
        r"""
        Planck spectral radiance \(B_\lambda(T)\).

        Formula (Planck 1901):

        .. math::
           B_\lambda =
           \frac{2 h c^2}{\lambda^5}
           \frac{1}{e^{\frac{hc}{\lambda k_B T}} - 1}

        Units: W m⁻² sr⁻¹ m⁻¹.
        """
        x = (h * c) / (lam_m * kB * T)
        return (2 * h * c**2) / (lam_m**5) / np.expm1(x)

    def _flux_mJy(self, D_m, Geocentric_AU, T, eps):
        r"""
        Flux density \(F_\nu\) from a thermally emitting asteroid.

        Based on the WISE/NEOWISE thermal flux model  
        (Wright et al. 2010; Mainzer et al. 2011):

        Steps
        -----
        1. Radiance \(B_\lambda(T)\) from Planck (1901).

        2. Hemispherical exitance:

           .. math::
              M_\lambda = \varepsilon \pi B_\lambda.

        3. Flux at observer (radius \(R=D/2\), distance \(\Delta\)):

           .. math::
              F_\lambda = M_\lambda \frac{R^2}{\Delta^2}.

        4. Convert wavelength → frequency
           (Allen, *Astrophysical Quantities*, 4th ed.):

           .. math::
              F_\nu = \frac{\lambda^2}{c} F_\lambda.

        5. Convert SI flux to mJy.

        References
        ----------
        - Planck (1901)
        - Lebofsky & Spencer (1989)
        - Wright et al. (2010)
        - Mainzer et al. (2011)
        - Allen (2000), *Astrophysical Quantities*
        """
        lam_m = self.lambda_um * 1e-6
        R_m = 0.5 * D_m
        Delta_m = Geocentric_AU * AU_M

        B_lam = self._planck_spectral_radiance(lam_m, T)
        F_lam = eps * np.pi * B_lam * (R_m**2) / (Delta_m**2)
        F_nu_SI = (lam_m**2 / c) * F_lam
        return F_nu_SI / MJY_W_M2_HZ

    def _single_exposure_prob(self, F_mJy, F_lim_mJy, k=0.5):
        r"""
        Logistic detection probability in log-flux space.

        Common in survey sensitivity models
        (e.g., mission simulation papers for WISE, LSST, Euclid).

        .. math::
           P =
           \frac{1}{1 +
           \exp\left(
             \frac{\log_{10} F_{\rm lim} - \log_{10}F}{k}
           \right)}.
        """
        F_clamped = np.maximum(F_mJy, 1e-40)
        logF = np.log10(F_clamped)
        logL = np.log10(F_lim_mJy)
        return 1.0 / (1.0 + np.exp((logL - logF) / k))

    def plot_detection_maps(
        self,
        D_range=None,
        Geocentric_range_AU=None,
        Heliocentric_AU=1.0,
        A_bond=0.10,
        eps=0.90,
        eta=1.0,
        k_fluxdex=0.25,
        cmap_single="viridis",
        cmap_24h="inferno",
    ):
        r"""
        Plot detection probability maps.

        Two panels:

        1. **Single-exposure detection probability**

           .. math::
              P_{\rm single}(D,\Delta).

        2. **24-hour detection probability**

           .. math::
              P_{24}(D,\Delta) = f_{\rm sky} P_{\rm single}.

        All fluxes and temperatures follow NEATM and WISE modeling:

        - Harris & Lagerros (2002)  
        - Wright et al. (2010)  
        - Mainzer et al. (2011)
        """
        if D_range is None:
            D_range = np.logspace(1, 3, 50)
        if Geocentric_range_AU is None:
            Geocentric_range_AU = np.linspace(0.05, 1.0, 50)

        Delta_AU, D_m = np.meshgrid(Geocentric_range_AU, D_range)

        T = self._equilibrium_temp(Heliocentric_AU, A_bond, eps, eta)
        Fnu = self._flux_mJy(D_m, Delta_AU, T, eps)
        P_single = self._single_exposure_prob(Fnu, self.Fnu_lim_mJy, k=k_fluxdex)

        f_sky = self.deg2perday / 41253.0
        P_24h = f_sky * P_single

        fig, ax = plt.subplots(figsize=(7, 5))
        im = ax.pcolormesh(Delta_AU, D_m, P_single,
                           shading="auto", cmap=cmap_single, vmin=0, vmax=1)
        ax.set_title(f"{self.name}\nSingle-Exposure Detection Probability")
        ax.set_xlabel("Geocentric Distance Δ (AU)")
        ax.set_ylabel("Diameter D (m)")
        ax.set_yscale("log")
        fig.colorbar(im, ax=ax)

        fig2, ax2 = plt.subplots(figsize=(7, 5))
        im2 = ax2.pcolormesh(Delta_AU, D_m, P_24h,
                             shading="auto", cmap=cmap_24h, vmin=0, vmax=1)
        ax2.set_title(f"{self.name}\n24h Detection Probability (f_sky={f_sky:.3f})")
        ax2.set_xlabel("Geocentric Distance Δ (AU)")
        ax2.set_ylabel("Diameter D (m)")
        ax2.set_yscale("log")
        fig2.colorbar(im2, ax=ax2)

