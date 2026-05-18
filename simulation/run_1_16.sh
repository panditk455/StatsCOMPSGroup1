#!/bin/bash
#SBATCH --job-name=sim_1_16
#SBATCH --array=1-16
#SBATCH --output=/Accounts/panditk/stats-comps/out_s%a.txt
#SBATCH --cpus-per-task=1
#SBATCH --mem=20G
#SBATCH --partition=SevenDay

cd /Accounts/panditk/stats-comps/
Rscript scenarios_1_16.R ${SLURM_ARRAY_TASK_ID}