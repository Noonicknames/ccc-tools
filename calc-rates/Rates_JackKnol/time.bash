#!/bin/bash

[[ -f time.out ]] && rm time.out

for t in $(seq 1 24)
do
	export OMP_NUM_THREADS=${t}
	echo $t $( srun -N 1 -n 1 -c ${t} ./main < input2 | grep 'TIME' | awk '{print $2}') >> time.out
	echo "t = ${t}"
done
