# Supplemental Code — JAMIA Open
[![DOI](https://zenodo.org/badge/1076565715.svg)](https://doi.org/10.5281/zenodo.17355295)

**Title:** Using Mixture Cure Models to Address Algorithmic Bias in Diagnostic Timing: Autism as a Test Case
**Purpose:** Reproduce the simulation study and figures reported in the manuscript.

## What’s here
- `code/01_requirements.R`: installs & loads required packages; records session info.
- `code/02_simulate.R`: core simulation and analysis functions.
- `code/03_run_all.R`: parameter grid, runs simulations, saves CSV outputs.
- `code/04_figures.R`: produces manuscript figures from saved CSVs.
- `data/`: (output) CSV files produced by `03_run_all.R`.
- `figs/`: (output) PNG figures produced by `04_figures.R`.
- `session-info/sessionInfo.txt`: R session info captured during a run.

## Quick start
1. Open R (≥ 4.2) on macOS/Linux/Windows.
2. Run, in order:
   - `source("code/01_requirements.R")`
   - `source("code/02_simulate.R")`
   - `source("code/03_run_all.R")`  # writes CSVs to `data/`
   - `source("code/04_figures.R")`  # writes PNGs to `figs/`
3. Figures should match those in the manuscript.
