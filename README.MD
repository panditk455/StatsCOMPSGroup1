# Getting Uncertainty Right: Bootstrap Methods for Clustered Forensic Firearm Examination Data

**Carleton College Statistics COMPS — Group 1**
Kritika Pandit, Cici Wang, Nicholas Chang
Advisor: Prof. Amanda Luby | Winter/Spring 2026

---

## Overview

Forensic firearm examination data have a two-level clustered structure: multiple examiners evaluate overlapping sets of cartridge case comparisons (csets). Standard bootstrap confidence intervals ignore this structure, understating uncertainty by 25–35 percentage points.

This project compares four bootstrap methods for constructing confidence intervals for the **error rate** and **inconclusive rate**:

| Method | Description |
|---|---|
| Naive (row) | Treats every observation as independent — ignores all clustering |
| Examiner-cluster | Resamples whole examiners |
| Cset-cluster | Resamples whole comparison sets |
| Two-way combined | Accounts for both levels simultaneously (Cameron et al., 2011) |

**Key finding:** The two-way combined bootstrap is the only method that consistently achieves near-nominal 95% coverage for both estimands across all 48 scenarios.

---

## Repository Structure

```
StatsCOMPSGroup1/
│
├── simulation/                       # Three-method simulation (naive, examiner, cset)
│   ├── simulation_functions.R        # Core functions — source this from all scripts
│   ├── scenarios_1_16.R              # Scenario runner: scenarios 1-16
│   ├── scenarios_17_32.R             # Scenario runner: scenarios 17-32
│   ├── scenarios_33_48.R             # Scenario runner: scenarios 33-48
│   ├── run_1_16.sh                   # SLURM job array: scenarios 1-16
│   ├── run_17_32.sh                  # SLURM job array: scenarios 17-32
│   └── run_33_48.sh                  # SLURM job array: scenarios 33-48
│
├── simulation_combined/              # Four-method simulation (adds combined bootstrap)
│   ├── combined_bootstrap.R          # Two-way combined bootstrap implementation
│   ├── simulation_functions.R        # Core functions for combined runs
│   ├── scenarios_1_16_combined.R     # Scenario runner: scenarios 1-16
│   ├── scenarios_17_32_combined.R    # Scenario runner: scenarios 17-32
│   ├── scenarios_33_48_combined.R    # Scenario runner: scenarios 33-48
│   ├── run_combined_1_16.sh          # SLURM job array: scenarios 1-16
│   ├── run_combined_17_32.sh         # SLURM job array: scenarios 17-32
│   └── run_combined_33_48.sh         # SLURM job array: scenarios 33-48
│
├── results/
│   ├── situation1_48_uncombined.csv  # Results: 3 methods x 48 scenarios
│   ├── combined_situation1_48.csv    # Results: 4 methods x 48 scenarios (used in paper)
│   └── compile into csv.Rmd          # Script to compile .rds outputs into CSVs
│
├── figures/
│   ├── generate_plots_combined.R     # Script to regenerate all paper figures
│   ├── visualization/                # Figures from three-method runs
│   │   ├── fig1_coverage_by_quality_haw0.png
│   │   ├── fig2_coverage_vs_hawthorne.png
│   │   ├── fig3_ci_width_vs_hawthorne.png
│   │   ├── fig4_heatmap_all_scenarios.png
│   │   ├── fig5_heatmap_inc_rate.png
│   │   ├── fig7_coverage_vs_width_scatter.png
│   │   └── fig8_slope_hawthorne.png
│   └── visualization_combined/       # All figures used in the paper (four-method)
│       ├── fig1_coverage_by_quality_haw0.png
│       ├── fig2_coverage_vs_hawthorne.png
│       ├── fig3_ci_width_vs_hawthorne.png
│       ├── fig4_heatmap_err_rate.png
│       ├── fig5_heatmap_inc_rate.png
│       ├── fig6_diverging_bar.png
│       ├── fig7a_naive_only.png
│       └── fig7b_all_methods.png
│
├── data/
│   └── bullets_2024.xlsx             # Raw Bullet Black Box Study data
│
└── documents/
    └── TeamContract.pdf
```

---

## The 48 Scenarios

Scenarios cross three factors:

| Factor | Levels |
|---|---|
| Case difficulty | Easy, Medium, Difficult, VeryDifficult |
| Hawthorne effect | H = 0.0, 0.2, 0.5 |
| Examiner quality | AA, BB, CC, CD |

**4 x 3 x 4 = 48 scenarios**, each run with M = 200 repetitions and B = 300 bootstrap resamples per repetition.

---

## How to Reproduce

### Step 1 — Run the simulation on HPC

```bash
# Three-method runs (naive, examiner, cset)
sbatch simulation/run_1_16.sh
sbatch simulation/run_17_32.sh
sbatch simulation/run_33_48.sh

# Four-method runs (adds combined bootstrap)
sbatch simulation_combined/run_combined_1_16.sh
sbatch simulation_combined/run_combined_17_32.sh
sbatch simulation_combined/run_combined_33_48.sh
```

Each job saves one `.rds` file per scenario (e.g., `result_s1.rds`).

### Step 2 — Compile results into CSV

```r
library(purrr)

# Three-method results
files <- list.files(pattern = "^result_s[0-9]+\\.rds$")
results <- map_dfr(files, ~ readRDS(.x)$table)
write.csv(results, "results/situation1_48_uncombined.csv", row.names = FALSE)

# Four-method (combined) results
files <- list.files(pattern = "^result_combined_s[0-9]+\\.rds$")
results <- map_dfr(files, ~ readRDS(.x)$table)
write.csv(results, "results/combined_situation1_48.csv", row.names = FALSE)
```

### Step 3 — Generate figures

```r
source("figures/generate_plots_combined.R")
```

---

## Model Parameters

All parameters are hardcoded in `simulation/simulation_functions.R`, estimated from the Bullet Black Box Study:

| Parameter | Value | Description |
|---|---|---|
| sigma_exam,inc | 1.1677 | Examiner SD — inconclusive model |
| sigma_cset,inc | 0.8605 | Cset SD — inconclusive model |
| sigma_exam,err | 0.9655 | Examiner SD — error model |
| sigma_cset,err | ~0 | Cset SD — error model (negligible) |

---


GitHub: https://github.com/panditk455/StatsCOMPSGroup1
