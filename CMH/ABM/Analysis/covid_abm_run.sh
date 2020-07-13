  #!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
mpirun -n 1 R --vanilla < COVID_ABM_V2.R > Outputs/ABM_V2_Run.Rout