# Getting Uncertainty Right: Bootstrap Methods for Clustered Forensic Firearm Examination Data

**Carleton College Statistics COMPS - Group 1**

Kritika Pandit, Cici Wang, Nicholas Chang

Advisor: Prof. Amanda Luby | Spring 2026

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

## Data-Generating Model

Each simulated dataset mirrors the Bullet Black Box Study structure (49 examiners, 100 comparison items). Decisions are generated through a two-stage process:

**Stage 1 — Inconclusive or conclusive:**
Each examiner-cset pair (i, j) first determines whether the decision is inconclusive:

```
logit(p_inc,ij) = X_ij * beta_inc + u_i,inc + v_j,inc + 0.5 * H
```

where `u_i,inc ~ N(0, 1.1677^2)` is an examiner random effect, `v_j,inc ~ N(0, 0.8605^2)` is a cset random effect, and `H` is the Hawthorne parameter. If the decision is inconclusive, the process stops.

**Stage 2 — Error or correct (if conclusive):**
If the decision is conclusive, whether it is an error is drawn as:

```
logit(p_err,ij) = Z_ij * beta_err + u_i,err
```

where `u_i,err ~ N(0, 0.9655^2)`. No cset random effect is included here — item-level error variation was negligible in the real data.

All fixed-effect coefficients and variance components are hardcoded in `simulation/simulation_functions.R`, estimated from the real Bullet Black Box Study data.

---

## Bootstrap Methods

For each simulated dataset, four bootstrap confidence intervals are constructed using B = 300 resamples:

1. **Naive** — resamples individual examiner-cset rows with replacement, treating all observations as independent.
2. **Examiner-cluster** — resamples whole examiners with replacement, preserving within-examiner dependence.
3. **Cset-cluster** — resamples whole comparison sets with replacement, preserving within-item dependence.
4. **Two-way combined** — combines the three distributions above as:
   `theta*_combined = theta*_exam + theta*_cset - theta*_naive`
   This accounts for both clustering levels simultaneously (Cameron, Gelbach & Miller, 2011).

For each method and repetition, we record whether the 95% percentile interval covers the true parameter value. Coverage is evaluated across M = 200 repetitions per scenario.

---

## The 48 Scenarios

Scenarios cross three factors:

| Factor | Levels |
|---|---|
| Case difficulty | Easy, Medium, Difficult, VeryDifficult |
| Hawthorne effect | H = 0.0, 0.2, 0.5 |
| Examiner quality | AA, BB, CC, CD |

**4 x 3 x 4 = 48 scenarios**, each run with M = 200 repetitions and B = 300 bootstrap resamples.

Example scenario grid (first 5 of 48):

| Scenario | Difficulty | Hawthorne H | Quality1 | Quality2 |
|---|---|---|---|---|
| s1 | Easy | 0.0 | A | A |
| s2 | Easy | 0.0 | B | B |
| s3 | Easy | 0.0 | C | C |
| s4 | Easy | 0.0 | C | D |
| s5 | Easy | 0.2 | A | A |
| ... | ... | ... | ... | ... |

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
│   ├── scenarios_1_16_combined.R     # Scenario runner: scenarios 1-16
│   ├── scenarios_17_32_combined.R    # Scenario runner: scenarios 17-32
│   ├── scenarios_33_48_combined.R    # Scenario runner: scenarios 33-48
│   ├── run_combined_1_16.sh          # SLURM job array: scenarios 1-16
│   ├── run_combined_17_32.sh         # SLURM job array: scenarios 17-32
│   └── run_combined_33_48.sh         # SLURM job array: scenarios 33-48
│
├── results/
│   ├── situation1_48.csv             # Results: 3 methods x 48 scenarios
│   └── combined_situation1_48.csv    # Results: 4 methods x 48 scenarios (used in paper)
│
├── figures/
│   ├── generate_plots_combined.R     # Script to regenerate all paper figures
│   └── visualization_combined/       # All figures used in the paper
│       ├── fig4_heatmap_err_rate.png
│       ├── fig5_heatmap_inc_rate.png
│       ├── fig7b_all_methods.png
│       └── ...
│
├── data/
│   └── bullets_2024.xlsx             # Raw Bullet Black Box Study data
│
└── documents/
    └── TeamContract.pdf
```

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

Each job saves one `.rds` file per scenario (e.g., `result_s1.rds`). Reproducibility is ensured by `set.seed(42 + m)` at the start of each repetition.

### Step 2 — Compile results into CSV

```r
library(purrr)

# Three-method results
files <- list.files(pattern = "^result_s[0-9]+\\.rds$")
results <- map_dfr(files, ~ readRDS(.x)$table)
write.csv(results, "results/situation1_48.csv", row.names = FALSE)

# Four-method (combined) results
files <- list.files(pattern = "^result_combined_s[0-9]+\\.rds$")
results <- map_dfr(files, ~ readRDS(.x)$table)
write.csv(results, "results/combined_situation1_48.csv", row.names = FALSE)
```

### Step 3 — Generate figures

```r
source("figures/generate_plots_combined.R")
# Saves all figures to figures/visualization_combined/
```

---

## Model Parameters

All parameters hardcoded in `simulation/simulation_functions.R`, estimated from the Bullet Black Box Study:

| Parameter | Value | Description |
|---|---|---|
| sigma_exam,inc | 1.1677 | Examiner SD — inconclusive model |
| sigma_cset,inc | 0.8605 | Cset SD — inconclusive model |
| sigma_exam,err | 0.9655 | Examiner SD — error model |
| sigma_cset,err | ~0 | Cset SD — error model (negligible) |

---
