#!/bin/bash
#SBATCH --job-name=comb_17_32
#SBATCH --array=17-32
#SBATCH --output=/Accounts/panditk/stats-comps/out_comb_s%a.txt
#SBATCH --cpus-per-task=1
#SBATCH --mem=20G
#SBATCH --partition=SevenDay

cd /Accounts/panditk/stats-comps/
Rscript scenarios_17_32_combined.R ${SLURM_ARRAY_TASK_ID}
