import numpy as np
import matplotlib.pyplot as plt


class GroundSensor:
    r"""
    Simple ground-based optical survey sensor model.

    This class uses standard asteroid photometric relations to estimate
    detection probability in a given filter with a specified 5σ limiting
    magnitude and nightly sky coverage.

    The core pieces are:

    - Diameter–albedo–absolute magnitude relation:

      .. math::

         D = \frac{1329~{\rm km}}{\sqrt{p_V}} 10^{-H/5},

      inverted to compute :math:`H(D, p_V)` as used by
      JPL CNEOS "Asteroid Size Estimator" and many asteroid
      thermal/photometric works
      (see e.g. Ali-Lagoa et al. 2018, A&A 613, A37, Eq. (2),
      citing Pravec & Harris 2007).

    - The H–G photometric system and the approximation

      .. math::

         m \approx H + 5\log_{10}(r \Delta),

      here simplified into a single distance-like factor :math:`R_{\rm AU}`
      (Bowell et al. 1989, "Application of photometric models to asteroids",
      in *Asteroids II*).

    - A logistic detection probability in magnitude space, centered on the
      5σ limiting magnitude, following standard survey-sensitivity modeling
      practice (e.g. Monte Carlo survey simulations for NEO searches).

    References (for the physics)
    -----------------------------
    - Bowell, E., et al. (1989), "Application of photometric models to
      asteroids", *Asteroids II*.
    - Pravec, P. & Harris, A. W. (2007), Icarus 190, 250–259.
    - Ali-Lagoa, V., et al. (2018), A&A 613, A37.
    - JPL CNEOS "Asteroid Size Estimator".
    - General magnitude definitions: e.g. Pogson scale and limiting magnitude
      discussions in standard texts and review articles.
    """

    def __init__(
        self,
        name: str,
        m_fivesigma: float,
        deg2pernight: float,
        fixedcost: float,
        reccost: float,
    ) -> None:
        r"""
        Initialize an optical ground-based survey sensor.

        Parameters
        ----------
        name : str
            Name of the instrument or survey (e.g. ``"Pan-STARRS"``).
        m_fivesigma : float
            5σ limiting magnitude in the relevant band. This is the faintest
            point source detectable at about S/N ≈ 5 in a single exposure
            (see typical survey depth definitions and limiting-magnitude
            discussions in the literature).
        deg2pernight : float
            Effective sky area covered per night, in square degrees.
        fixedcost : float
            Fixed cost of the facility (for economic comparisons).
        reccost : float
            Recurring annual or per-year cost (for operations).
        """
        self.name = name
        self.m_fivesigma = m_fivesigma
        self.deg2pernight = deg2pernight
        self.fixedcost = fixedcost
        self.reccost = reccost

    def detection_prob_24h(self, D_m: float, R_AU: float, p_v: float) -> float:
        r"""
        Compute 24-hour detection probability for a single asteroid.

        Conceptually:

        1. Convert diameter and albedo to absolute magnitude :math:`H`:

           From the standard relation

           .. math::

              D = \frac{1329~{\rm km}}{\sqrt{p_V}} 10^{-H/5}

           (e.g. Pravec & Harris 2007; Ali-Lagoa et al. 2018; JPL CNEOS),
           we invert to get

           .. math::

              H(D, p_V) =
              5 \log_{10}\left(\frac{1329~{\rm km}}
              {D_{\rm km} \sqrt{p_V}}\right).

        2. Convert :math:`H` to apparent magnitude :math:`m` with a
           simplified distance dependence:

           .. math::

              m \approx H + 5 \log_{10}(R_{\rm AU}),

           where :math:`R_{\rm AU}` represents the effective distance factor
           (often :math:`r \Delta`, here collapsed into a single variable)
           as in the H–G based brightness approximations
           (Bowell et al. 1989).

        3. Convert magnitude to a single-exposure detection probability
           via a logistic function centered on the 5σ limit
           :math:`m_{5\sigma}`.

        4. Scale by sky coverage fraction:

           .. math::

              f_{\rm sky} = \frac{\text{deg2pernight}}{41253},

           assuming a full sky of 41,253 deg², and

           .. math::

              P_{\rm 24h} = f_{\rm sky} P_{\rm single}.

        Parameters
        ----------
        D_m : float
            Diameter in meters.
        R_AU : float
            Effective distance factor in AU (e.g., product or proxy of
            heliocentric and geocentric distances).
        p_v : float
            Geometric albedo :math:`p_V`.

        Returns
        -------
        float
            24-hour detection probability :math:`P_{\rm 24h} \in [0,1]`.
        """
        H = self._absolute_magnitude(D_m, p_v)
        m = self._apparent_magntitude(H, R_AU)
        P_single = self._single_exposure_prob(m, self.m_fivesigma)
        f_sky = self.deg2pernight / 41253.0
        return P_single * f_sky

    def _absolute_magnitude(self, D_m: float, p_v: float) -> float:
        r"""
        Compute absolute magnitude :math:`H` from diameter and albedo.

        Using the standard asteroid relation (e.g. Pravec & Harris 2007;
        Ali-Lagoa et al. 2018; JPL CNEOS):

        .. math::

           D = \frac{1329~{\rm km}}{\sqrt{p_V}} 10^{-H/5},

        which can be inverted to:

        .. math::

           H(D, p_V) =
           5 \log_{10}\!\left(
             \frac{1329~{\rm km}}{D_{\rm km}\sqrt{p_V}}
           \right).

        Parameters
        ----------
        D_m : float
            Diameter in meters.
        p_v : float
            Geometric albedo :math:`p_V`.

        Returns
        -------
        float
            Absolute magnitude :math:`H`.
        """
        D_km = D_m / 1000.0
        return 5 * np.log10(1329.0 / (D_km * np.sqrt(p_v)))

    def _apparent_magntitude(self, H: float, R_AU: float) -> float:
        r"""
        Compute apparent magnitude :math:`m` from :math:`H` and distance term.

        In the full H–G formalism (Bowell et al. 1989), the apparent
        magnitude of an asteroid depends on heliocentric distance
        :math:`r`, geocentric distance :math:`\Delta`, and phase angle
        :math:`\alpha`:

        .. math::

           m \approx H + 5 \log_{10}(r \Delta) + \Phi(\alpha),

        where :math:`\Phi(\alpha)` is a phase-function term.

        Here we use a **simplified** version that collapses the
        distance dependence into a single effective factor
        :math:`R_{\rm AU}` and neglects the phase function:

        .. math::

           m \approx H + 5 \log_{10}(R_{\rm AU}).

        Parameters
        ----------
        H : float
            Absolute magnitude.
        R_AU : float
            Effective distance factor in AU (e.g., proxy for :math:`r \Delta`).

        Returns
        -------
        float
            Approximate apparent magnitude :math:`m`.
        """
        return H + 5 * np.log10(R_AU)

    def _single_exposure_prob(
        self,
        m: float,
        m_fivesigma: float,
        k: float = 0.5,
    ) -> float:
        r"""
        Logistic single-exposure detection probability in magnitude space.

        The limiting magnitude :math:`m_{5\sigma}` represents the faintest
        source detectable at approximately S/N ≈ 5; detection efficiency
        near this limit is not a hard step function, so we approximate it
        by a logistic curve in magnitude:

        .. math::

           P_{\rm single}(m) =
           \frac{1}{1 + \exp\left(\frac{m - m_{5\sigma}}{k}\right)}.

        This is an empirical/heuristic choice commonly used in
        survey simulations and instrument-performance modeling, providing
        a smooth transition from bright (probability ≈ 1) to faint
        (probability ≈ 0).

        Parameters
        ----------
        m : float
            Apparent magnitude of the source.
        m_fivesigma : float
            5σ limiting magnitude :math:`m_{5\sigma}`.
        k : float
            Width parameter for the logistic transition in magnitudes.

        Returns
        -------
        float
            Single-exposure detection probability :math:`P_{\rm single}`.
        """
        return 1.0 / (1.0 + np.exp((m - m_fivesigma) / k))

    def plot_detection_maps(
        self,
        D_range=None,
        R_range_AU=None,
        p_v: float = 0.14,
        k_mag: float = 0.5,
        cmap_single: str = "viridis",
        cmap_24h: str = "inferno",
    ) -> None:
        r"""
        Plot detection probability as a function of size and distance.

        Two panels are produced in the :math:`(D, R_{\rm AU})` plane:

        1. **Single-exposure detection probability**

           .. math::

              P_{\rm single}(D, R_{\rm AU}).

        2. **24-hour detection probability**

           .. math::

              P_{24}(D, R_{\rm AU}) =
              f_{\rm sky}\,P_{\rm single}(D, R_{\rm AU}), \quad
              f_{\rm sky} = \frac{\text{deg2pernight}}{41253}.

        The underlying relations are the same as those used in
        :meth:`detection_prob_24h`, based on the standard asteroid
        photometric formulae (Bowell et al. 1989; Pravec & Harris 2007;
        Ali-Lagoa et al. 2018; JPL CNEOS).

        Parameters
        ----------
        D_range : array-like, optional
            Diameters in meters; default is log-spaced 10–1000 m.
        R_range_AU : array-like, optional
            Effective distance factors in AU; default is 0.05–1.0 AU linear.
        p_v : float, optional
            Geometric albedo :math:`p_V` (default 0.14 ≈ typical NEA value).
        k_mag : float, optional
            Logistic roll-off width in magnitudes.
        cmap_single : str
            Matplotlib colormap name for the single-exposure panel.
        cmap_24h : str
            Matplotlib colormap name for the 24h panel.
        """
        if D_range is None:
            D_range = np.logspace(1, 3, 50)  # 10–1000 m
        if R_range_AU is None:
            R_range_AU = np.linspace(0.05, 1.0, 50)

        R_AU_grid, D_m_grid = np.meshgrid(R_range_AU, D_range)

        # Compute H(D), m(H, R), and probabilities on the grid
        H_grid = self._absolute_magnitude(D_m_grid, p_v)
        m_grid = self._apparent_magntitude(H_grid, R_AU_grid)
        P_single_grid = self._single_exposure_prob(
            m_grid, self.m_fivesigma, k=k_mag
        )

        f_sky = self.deg2pernight / 41253.0
        P_24h_grid = f_sky * P_single_grid

        # Panel 1: single-exposure probability
        fig, ax = plt.subplots(figsize=(7, 5))
        im1 = ax.pcolormesh(
            R_AU_grid,
            D_m_grid,
            P_single_grid,
            shading="auto",
            cmap=cmap_single,
            vmin=0.0,
            vmax=1.0,
        )
        ax.set_title(f"{self.name}\nSingle-Exposure Detection Probability")
        ax.set_xlabel("Effective Distance $R_{\\rm AU}$")
        ax.set_ylabel("Diameter $D$ (m)")
        ax.set_yscale("log")
        fig.colorbar(im1, ax=ax, label="$P_{\\rm single}$")

        # Panel 2: 24-hour probability
        fig2, ax2 = plt.subplots(figsize=(7, 5))
        im2 = ax2.pcolormesh(
            R_AU_grid,
            D_m_grid,
            P_24h_grid,
            shading="auto",
            cmap=cmap_24h,
            vmin=0.0,
            vmax=1.0,
        )
        ax2.set_title(
            f"{self.name}\n24 h Detection Probability "
            f"(f_sky = {f_sky:.3f})"
        )
        ax2.set_xlabel("Effective Distance $R_{\\rm AU}$")
        ax2.set_ylabel("Diameter $D$ (m)")
        ax2.set_yscale("log")
        fig2.colorbar(im2, ax=ax2, label="$P_{24\\,\\rm h}$")
