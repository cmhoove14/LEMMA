#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
mpirun -n 1 R --vanilla < ABM_V2_parallel.R > Outputs/ABM_V2_Prl_Run.Rout