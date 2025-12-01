<!-- markdownlint-disable MD013 -->

# Validating Proximal Motion Equations

We take the equations from [Optimal Impact Strategies for Asteroid Deflection](https://arc.aiaa.org/doi/10.2514/1.33432). We assume that both a NEO of interest and the Earth has been imported into STK, if not, consult [horizons_to_stk.md](../horizons_preprocessing/horizons_to_stk.md).
We use the NEO "2001 WN5" in this example.

## Calculating Time of Closest Approach

Right-click 2001 WN5 and open the Analysis Workbench. We must create a `Calculation` and `Time Instant` Object.

### Creating `Vector_Magnitude_2001WN5_to_Earth`

Go to the tab 'Calculation' and select 'Create New Scalar Calculation'. For the Type, select Vector Magnitude.

As the Input Vector, select 2001 WN5 in the mini Object Browser than opens and choose To Vectors/Earth.

> **Creating a Report or Graph**\
> Right-click 2001 WN5 in the Object Browser and open Report & Graph Manager\
> There, you can create a new report or graph, and include `Vector_Magnitude_2001WN5_to_Earth` as the data source
> ![Asteroid diagram](images/distance_graph.jpg)

### Creating `Minimum_of_Vector_Magnitude_2001WN5_to_Earth`

In the Analysis Workbench, switch to the tab 'Time'. There, create a new time instant. For the Type, select 'Time of Extremum'. As the Calculation, select `Vector_Magnitude_2001WN5_to_Earth`.

> **Creating a Report or Graph**
> Similarly, create a Report using `Minimum_of_Vector_Magnitude_2001WN5_to_Earth` and the result should look like:\
> ![Asteroid diagram](images/closest_time.png)\
> This should match the closest approach time given by the CNEOS website

## Finding State Vector at Closest Approach

Create a new report called "State_Vectors_at_Closest_Approach". This should include the following data:\
![Asteroid diagram](images/state_report_contents.png)

In the Report & Graph Manager, before generating the report, we must adjust the time. In the bottom left, select 'Specify Time Properties' and it to 'Custom Times', `2001WN5 Vector_Magnitude_2001WN5_to_Earth.TimesOfLocalMin`. Finally, generate the report:

![alt text](images/state_report.png)
