<!-- markdownlint-disable MD013 -->

# Python MCDP

## Setup

Create a Python virtual environment and activate it. Install all required packages using:

```bash
pip install -r requirements.txt
```

## Usage

[`test_mcdp.py`](testing/test_mcdp.py) contains all code needed to create Pareto fronts. All setup, including asteroid probability distributions, sensors, and risk parameters can be adjusted directly in the file. Asteroid probability distributions are taken from [NEO Population, Velocity Bias, and Impact Risk from an ATLAS Analysis](https://iopscience.iop.org/article/10.3847/PSJ/abd325) by A. N Heinze et al.

[`test_mcdp_parallel.py`](testing/test_mcdp_parallel.py) is a significantly sped-up version of the MCDP algorithm that uses multithreading. It is recommended for large state-spaces.

Other testing functions can be used to generate sensor plots and hazard curves.
