#!/bin/bash
#SBATCH --job-name=sim_33_48
#SBATCH --array=33-48
#SBATCH --output=/Accounts/panditk/stats-comps/out_s%a.txt
#SBATCH --cpus-per-task=1
#SBATCH --mem=20G
#SBATCH --partition=SevenDay

cd /Accounts/panditk/stats-comps/
Rscript scenarios_33_48.R ${SLURM_ARRAY_TASK_ID}
