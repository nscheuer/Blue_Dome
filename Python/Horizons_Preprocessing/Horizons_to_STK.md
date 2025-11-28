# Horizons to STK Conversion
## Determine Object of Interest
This can be done, for instance, by searching on the [Center for Near-Earth Object Studies](https://cneos.jpl.nasa.gov/) in the [Close Approaches](https://cneos.jpl.nasa.gov/ca/) section.

Specifically, all asteroids having a minimum orbit intersection distance (MOID) smaller than 0.05 AU and an absolute magnitude of 22 or less are considered a potentially hazardous asteroid (PHA). (From [Optimal Impact Strategies for Asteroid Deflection](https://arc.aiaa.org/doi/10.2514/1.33432))

For instance, we may search with settings "Future Only", "Nominal dist. <= 1LD" and "H <= 26" to find a list of 43 entries, including [143814 (2001 WN5)](https://ssd.jpl.nasa.gov/tools/sbdb_lookup.html#/?sstr=153814) with a close approach in $2028-Jun-26 \quad 05:23 \pm 00:01$ and a minimum distance of 0.00166 AU. 

## Producing Horizons Ephemeris
After determining the object of interest, we may use [Horizons Systems](https://ssd.jpl.nasa.gov/horizons/app.html#/) to propagate the orbit and produce an ephemeris. Use the following settings:

> ### **Horizons Web-Interface Settings**
> **Ephemeris Type:** Vector Table  
> **Target Body:** 153814 (2001 WN5)  
> **Coordinate Center:** Sun (body center) [500@10]  
> **Timespan:** 2028-01-01 → 2028-12-31  
> **Step Size:** 1 hours  
> **Select Output Quantities:** 2. State Vector {x,y,zVx,Vy,Vz}  
> **Reference Frame:** ICRF  
> **Reference Plane:** x-y axes of reference frame (equatorial or equatorial-aligned, inertial)  
> **Vector Correction:** geometric states  
> **Calendar type:** Gregorian  
> **Output units:** km and seconds  
> **Vector labels:** False  
> **Output TDB-UT:** False  
> **CSV Format:**  True  
> **Object Summary:**  False

The timespan should be adjusted accordingly based on the simulation time and MOID time.

After the ephemeris is produced, save it as a `.txt` file, optionally in `Python/Horizons_Preprocessing/horizons_files`.

## Producing Earth Ephemeris
Similarly to the NEO, the Earth Ephemeris for the timespan of interest can be calculated using the same settings as above. 

## Converting Results to STK Ephemeris `.e` file
Adjust the filepath in `horizons_to_stk.py` to reflect the intended ephemeris to be converted and run the Python script. 

## Importing into STK
Create a new STK Scenario and set the Start and Stop times to reflect the timespan of the generated ephemeris. 

> **⚠️ Important!**  
> Make sure to set `Central Body` to `Sun` in the New Scenario Wizard.

Following this, press `+` and import a new satellite using the Ephemeris option. It might take a while for the orbit track to appear. 

> **Reloading Ephemeris**  
> Sometimes it helps to select the satellite in the Object Browser, right-clicking to Properties, and reloading the Ephemeris. 

![Asteroid diagram](images/only_neo.png)

To visualize the position of the satellite, go into Properties>3D Graphics>Model and set the Detail Thresholds to the maximum.

![Asteroid diagram](images/both.png)