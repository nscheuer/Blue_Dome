# Blue Dome
MIT 1.144 [ACT4ED](https://zardini.mit.edu/act4ed/) Project, Fall 2025

## About
Applied Category theory offers new opportunities to approach complex design problems in a heuristic and tractable way. Properties such as compositionality allow for design problems to be split into smaller problems, solved, and recombined into a provably optimal solution. 

**Blue Dome** is our Fall 2025 ACT4ED project using category theory to investigate **orbital defense**. Specifically, we investigate **sensor selection** and **interception methods**. 

## Organization of the Repository
### Presentations
Presentations on our work can be found [here](/present). 

### Sensor Modelling
Sensor modelling for ground-based and space-based sensors can be found [here](/Python/Detection_Modelling/).

![Sensor Modelling](/images/sensor_modelling.jpg)

### Sensor Selection (Python)
Python code on determining sensor choices can be found in [Python_MCDP](/Python_MCDP/README.md).

![Python MCDP](/images/Python_MCDP.png)

### Sensor Selection (MCDPL)
MCDPL code on determining sensor choices can be found in [dome.mcdplib](/dome.mcdplib/).

![MCDPL](/images/MCDPL.png)

### STK Simulations
ANSYS Satellite Toolbox is used for visualization of orbits and sensors. Tools to convert [JPL Horizons Ephemeris](https://ssd.jpl.nasa.gov/horizons/app.html#/) into ANSYS STK Ephemeris to create orbits of NEOs can be found in [horizons_to_stk.md](/Python/Horizons_Preprocessing/Horizons_to_STK.md).

### Proximal Motion Equations
Initial work was done on determining orbital changes based on kinetic intercepts. Equations are from [Optimal Impact Strategies for Asteroid Deflection](https://arc.aiaa.org/doi/10.2514/1.33432) by Massimiliano Vasile and Camilla Colombo.


