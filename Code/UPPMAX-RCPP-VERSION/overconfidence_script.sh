#!/bin/bash -l

#SBATCH -A snic2020-15-262
#SBATCH -p node
#SBATCH -n 1
#SBATCH -t 24:00:00
#SBATCH -J overconfidence
#SBATCH --mail-user=oscar.oelrich@stat.su.se
#SBATCH --mail-type=END,FAIL
module load R/4.0.0
"/proj/dennis/test-r/test_temp.R"