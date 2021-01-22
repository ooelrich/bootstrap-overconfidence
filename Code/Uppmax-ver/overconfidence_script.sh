#!/bin/bash -l

#SBATCH -A snic2020-15-262
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 01:00:00
#SBATCH -J overconfidence
#SBATCH -a 1-201
#SBATCH --mail-user=oscar.oelrich@stat.su.se
#SBATCH --mail-type=END,FAIL
export N_CORES="1"
export N_OBS="1000"
module load R/4.0.0
module load R_packages/4.0.0

Rscript batch_version.R