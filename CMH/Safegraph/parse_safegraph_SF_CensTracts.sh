#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
mpirun -n 1 R --vanilla < parse_safegraph_SF_CensTracts.R > safegraph_parse_SF_CensTracts.Rout