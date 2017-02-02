#!/bin/bash
readonly MATRIX_SIZE=500
for i in 0.0625 0.125 0.25 0.5 0.75 0.875 0.9375
do
    ./crs_bicg $MATRIX_SIZE $i > "data/$MATRIX_SIZE_$i.dat"
done
