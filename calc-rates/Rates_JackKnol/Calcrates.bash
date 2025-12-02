#!/bin/bash -l
#SBATCH --account=d35
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=2:00:00
#SBATCH --export=NONE

# export OMP_NUM_THREADS=24

targetmol="B+"
datadir="tmp"
vmax=1

for file in $( ls ${datadir}/Cross_sections/vi=*/*_vi=*.txt | cut -d '/' -f 4 | rev | cut -d '_' -f 2- | rev | sort -u)
do
  echo ${targetmol} > input2
    echo ${datadir} >> input2
       echo ${file} >> input2
         echo ${vmax} >> input2   
        ./main < input2
        # srun -N 1 -n 1 -c 24 ./main < input2
done


